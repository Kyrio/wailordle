module Main exposing (main)

import Array
import Browser
import Browser.Dom
import Dict
import Html exposing (Html, div, button, input, text, br, span, h1, h2)
import Html.Attributes exposing (class, id, type_, size, placeholder, spellcheck, value, autocomplete, style, title, disabled)
import Html.Events exposing (onInput, onClick)
import Http
import Random
import Random.List
import String.Normalize exposing (removeDiacritics)
import Task
import Url.Builder exposing (relative)

import Game exposing (..)


type Model
  = LoadingPokemonTable
  | LoadingPokemonByName PokemonTable
  | Failure
  | Ready (PokemonTable, PokemonByName)
  | Chosen GameData


type Signal
  = ReceivedPokemonTable (Result Http.Error PokemonTable)
  | ReceivedPokemonByName (Result Http.Error PokemonByName)
  | ChosePokemon (Maybe Pokemon, List Pokemon)
  | Typed String
  | Submitted Pokemon
  | EnteredBacklog Pokemon
  | Rerolled
  | GaveUp
  | NoOp


main : Program () Model Signal
main =
  Browser.element
    { init = init
    , update = update
    , subscriptions = subscriptions
    , view = view
    }


init : () -> (Model, Cmd Signal)
init _ =
  ( LoadingPokemonTable
  , Http.get
      { url = relative [ "assets", "json", "pokemon_list.json" ] []
      , expect = Http.expectJson ReceivedPokemonTable pokemonTableDecoder
      }
  )


update : Signal -> Model -> (Model, Cmd Signal)
update signal model =
  case signal of
    ReceivedPokemonTable result ->
      case result of
        Ok table ->
          ( LoadingPokemonByName table
          , Http.get
              { url = relative [ "assets", "json", "pokemon_by_french_name.json" ] []
              , expect = Http.expectJson ReceivedPokemonByName pokemonByNameDecoder
              }
          )
        Err _ ->
          (Failure, Cmd.none)

    ReceivedPokemonByName result ->
      case result of
        Ok byName ->
          case model of
            LoadingPokemonByName table ->
              let
                pokemonPool = Dict.values table
              in
                ( Ready (table, byName)
                , Random.generate ChosePokemon (Random.List.choose pokemonPool)
                )
            _ ->
              (model, Cmd.none)
        Err _ ->
          (Failure, Cmd.none)

    ChosePokemon (maybe, rest) ->
      case model of
        Ready (table, byName) ->
          case maybe of
            Just pokemon ->
              (Chosen
                { status = Guessing
                , pokemonTable = table
                , pokemonByName = byName
                , genPokemonByName = List.filter (filterByGen pokemon.species.generation table) byName
                , pokemonPool = rest
                , chosen = pokemon
                , search = ""
                , searchResults = []
                , guesses = []
                , activeGuess = Nothing
                }
              , Task.attempt (\_ -> NoOp) (Browser.Dom.focus "search-bar")
              )
            Nothing ->
              (model, Cmd.none)
        _ ->
          (model, Cmd.none)

    Typed search ->
      case model of
        Chosen gameData ->
          let
            results =
              if String.isEmpty search then
                []
              else
                List.filter (filterByName search) gameData.genPokemonByName
          in
            (Chosen { gameData | search = search, searchResults = results }, Cmd.none)
        _ ->
          (model, Cmd.none)

    Submitted pokemon ->
      case model of
        Chosen gameData ->
          if gameData.status == Guessing then
            ( Chosen
                { gameData
                | guesses = gameData.guesses ++ [ pokemon ]
                , activeGuess = Just pokemon
                , search = ""
                , searchResults = []
                , status =
                    if pokemon.identifier == gameData.chosen.identifier then
                      Succeeded
                    else
                      Guessing
                }
            , Task.attempt (\_ -> NoOp) (Browser.Dom.focus "search-bar")
            )
          else
            (model, Cmd.none)
        _ ->
          (model, Cmd.none)

    EnteredBacklog guess ->
      case model of
        Chosen gameData ->
          ( Chosen { gameData | activeGuess = Just guess }
          , Cmd.none
          )
        _ ->
          (model, Cmd.none)

    Rerolled ->
      case model of
        Chosen gameData ->
          ( Ready (gameData.pokemonTable, gameData.pokemonByName)
          , Random.generate ChosePokemon (Random.List.choose gameData.pokemonPool)
          )
        _ ->
          (model, Cmd.none)

    GaveUp ->
      case model of
        Chosen gameData ->
          ( Chosen
              { gameData
              | guesses = gameData.guesses ++ [ gameData.chosen ]
              , activeGuess = Just gameData.chosen
              , search = ""
              , searchResults = []
              , status = Failed
              }
          , Cmd.none
          )
        _ ->
          (model, Cmd.none)

    NoOp ->
      (model, Cmd.none)


subscriptions : Model -> Sub Signal
subscriptions _ =
  Sub.none


view : Model -> Html Signal
view model =
  div [ class "app" ]
    ( case model of
        LoadingPokemonTable ->
          viewLoading
        LoadingPokemonByName _ ->
          viewLoading
        Failure ->
          viewFailure
        Ready _ ->
          viewReady
        Chosen gameData ->
          viewGame gameData
    )


viewGame : GameData -> List (Html Signal)
viewGame gameData =
  [ div [ class "prompt" ]
      [ text "Je pense à un Pokémon de la "
      , span [ class "generation" ] [ text ("Génération " ++ String.fromInt gameData.chosen.species.generation) ]
      , text (getSpecialFormHint gameData.chosen.identifier)
      , text "."
      ]
  , div [ class "toolbar" ]
    [ button [ class "tool-button", type_ "button", disabled (gameData.status /= Guessing), onClick GaveUp ]
        [ span [ class "tool-icon give-up" ] []
        , text "Je ne sais pas"
        ]
    , button [ class "tool-button", type_ "button", onClick Rerolled ]
        [ span [ class "tool-icon reroll" ] []
        , text "Nouveau tirage"
        ]
    ]
  , div [ class "guesses" ]
      ( case gameData.activeGuess of
          Nothing ->
            [ viewEmptyGuess ]
          Just active ->
            [ viewGuess gameData.status gameData.chosen active
            , viewBacklog gameData.status gameData.chosen active gameData.guesses ]
      )
  , div [ class "pokemon-search" ]
      [ input
        [ type_ "search"
        , id "search-bar"
        , size 30
        , placeholder "Entrez le nom d'un Pokémon..."
        , spellcheck False
        , autocomplete False
        , onInput Typed
        , value gameData.search
        ]
        []
      , div [ class "search-icon" ] []
      , div [ class "results" ]
          (List.map (mapSearchResult gameData.pokemonTable) gameData.searchResults)
      ]
  ]


viewBacklog : GameStatus -> Pokemon -> Pokemon -> List Pokemon -> Html Signal
viewBacklog status chosen activeGuess guesses =
  div [ class "backlog" ] (List.map (viewBacklogBall status chosen activeGuess) guesses)


viewBacklogBall : GameStatus -> Pokemon -> Pokemon -> Pokemon -> Html Signal
viewBacklogBall status chosen active guess =
  let
    c1 = "backlog-ball"
    c2 = if guess.identifier == active.identifier then " active" else ""
    c3 =
      if guess.identifier /= chosen.identifier then
        ""
      else if status == Failed then
        " failure"
      else
        " victory"
    ballClass = c1 ++ c2 ++ c3
  in
    div [ class ballClass, title guess.species.names.fr, onClick (EnteredBacklog guess) ] []


viewEmptyGuess : Html Signal
viewEmptyGuess =
  div [ class "guess" ]
    [ div [ class "guessline" ] [ div [ class "ribbon" ] [], div [ class "check" ] [] ]
    , div [ class "guessline" ] [ div [ class "ribbon" ] [], div [ class "check" ] [] ]
    , div [ class "guessline" ] [ div [ class "ribbon" ] [], div [ class "check" ] [] ]
    , div [ class "guessline" ] [ div [ class "ribbon" ] [], div [ class "check" ] [] ]
    , div [ class "guess-footer" ]
      [ div [ class "guess-pokemon" ]
          [ div [ class "empty-pokemon" ] []
          ]
      , div [ class "guess-name" ]
          [ h1 [] [ text "Non choisi" ]
          , h2 [] [ text "Faites un essai !" ]
          ]
      ]
    ]


viewGuess : GameStatus -> Pokemon -> Pokemon -> Html Signal
viewGuess status chosen guessed =
  let
    chosenTypeA = Maybe.withDefault "none" (Array.get 0 chosen.types)
    chosenTypeB = Maybe.withDefault "none" (Array.get 1 chosen.types)
    guessedTypeA = Maybe.withDefault "none" (Array.get 0 guessed.types)
    guessedTypeB = Maybe.withDefault "none" (Array.get 1 guessed.types)

    testTypeA =
      if guessedTypeA == chosenTypeA then
        div [ class "guessline", title "Type à la bonne position" ]
          [ div [ class "ribbon", styleTypeRibbon guessedTypeA ] []
          , div [ class "check correct" ] []
          ]
      else if guessedTypeA == chosenTypeB then
        div [ class "guessline", title "Type à la mauvaise position" ]
          [ div [ class "ribbon", styleTypeRibbon guessedTypeA ] []
          , div [ class "check swap" ] []
          ]
      else
        div [ class "guessline", title "Type absent" ]
          [ div [ class "ribbon", styleTypeRibbon guessedTypeA ] []
          , div [ class "check wrong" ] []
          ]

    testTypeB =
      if guessedTypeB == chosenTypeB then
        div [ class "guessline", title "Type à la bonne position" ]
          [ div [ class "ribbon", styleTypeRibbon guessedTypeB ] []
          , div [ class "check correct" ] []
          ]
      else if guessedTypeB == chosenTypeA then
        div [ class "guessline", title "Type à la mauvaise position" ]
          [ div [ class "ribbon", styleTypeRibbon guessedTypeB ] []
          , div [ class "check swap" ] []
          ]
      else
        div [ class "guessline", title "Type absent" ]
          [ div [ class "ribbon", styleTypeRibbon guessedTypeB ] []
          , div [ class "check wrong" ] []
          ]

    testHeight =
      if guessed.height < chosen.height then
        div [ class "guessline", title (getPrettyHeight guessed.height ++ " (trop petit)") ]
          [ div [ class "ribbon too-short" ] []
          , div [ class "check too-low" ] []
          ]
      else if guessed.height > chosen.height then
        div [ class "guessline", title (getPrettyHeight guessed.height ++ " (trop grand)") ]
          [ div [ class "ribbon too-tall" ] []
          , div [ class "check too-high" ] []
          ]
      else
        div [ class "guessline", title (getPrettyHeight guessed.height ++ " (taille exacte)") ]
          [ div [ class "ribbon correct-height" ] []
          , div [ class "check correct" ] []
          ]

    testWeight =
      if guessed.weight < chosen.weight then
        div [ class "guessline", title (getPrettyWeight guessed.weight ++ " (trop léger)") ]
          [ div [ class "ribbon too-light" ] []
          , div [ class "check too-low" ] []
          ]
      else if guessed.weight > chosen.weight then
        div [ class "guessline", title (getPrettyWeight guessed.weight ++ " (trop lourd)") ]
          [ div [ class "ribbon too-heavy" ] []
          , div [ class "check too-high" ] []
          ]
      else
        div [ class "guessline", title (getPrettyWeight guessed.weight ++ " (poids exact)") ]
          [ div [ class "ribbon correct-weight" ] []
          , div [ class "check correct" ] []
          ]
  in
    div
      [ if guessed.identifier /= chosen.identifier then
          class "guess"
        else if status == Failed then
          class "guess failure"
        else
          class "guess victory"
      ]
      [ testTypeA
      , testTypeB
      , testHeight
      , testWeight
      , div [ class "guess-footer" ]
          [ div [ class "guess-pokemon" ]
              [ div [ class ("pokesprite pokemon " ++ guessed.identifier) ] []
              ]
          , div [ class "guess-name" ]
              [ h1 [] [ text guessed.species.names.fr ]
              , h2 [] [ text guessed.identifier ]
              ]
          , div [ class "status" ]
            [ if guessed.identifier /= chosen.identifier then
                text ""
              else if status == Failed then
                text "Dommage !"
              else
                text "Victoire !"
            ]
          ]
      ]


viewLoading : List (Html Signal)
viewLoading =
  [ div [ class "alert" ] [ text "Chargement..." ] ]


viewFailure : List (Html Signal)
viewFailure =
  [ div [ class "alert" ] [ text "Impossible de charger la liste de Pokémon.", br [][], text "Essayez plus tard." ] ]


viewReady : List (Html Signal)
viewReady =
  [ div [ class "alert" ] [ text "Recherche d'un Pokémon aléatoire..." ] ]


mapSearchResult : PokemonTable -> (String, List Int) -> Html Signal
mapSearchResult pokemonTable (name, variants) =
  div [ class "species" ] (List.map (mapVariant pokemonTable) variants)


mapVariant : PokemonTable -> Int -> Html Signal
mapVariant pokemonTable variant =
  let
    key = String.fromInt variant
  in
    case Dict.get key pokemonTable of
      Nothing ->
        div [ class "alert" ] [ text "Missingno" ]
      Just pokemon ->
        div [ class "variant", onClick (Submitted pokemon) ]
          [ div [ class "variant-name" ]
            [ h1 [] [ text pokemon.species.names.fr ]
            , h2 [] [ text pokemon.identifier ]
            ]
          , div [ class ("pokesprite pokemon " ++ pokemon.identifier) ] []
          ]


filterByGen : Int -> PokemonTable -> (String, List Int) -> Bool
filterByGen generation pokemonTable (_, variants) =
  case List.head variants of
    Nothing ->
      False
    Just variant ->
      let
        key = String.fromInt variant
      in
        case Dict.get key pokemonTable of
          Nothing ->
            False
          Just pokemon ->
            pokemon.species.generation == generation


filterByName : String -> (String, List Int) -> Bool
filterByName search (name, _) =
  String.startsWith
    ( search
        |> String.toLower
        |> removeDiacritics
    )
    ( name
        |> String.toLower
        |> removeDiacritics
    )


getPrettyHeight : Int -> String
getPrettyHeight height_dm =
  "Taille : " ++ (String.fromFloat (toFloat height_dm / 10)) ++ " m"


getPrettyWeight : Int -> String
getPrettyWeight weight_hg =
  "Poids : " ++ (String.fromFloat (toFloat weight_hg / 10)) ++ " kg"


styleTypeRibbon : String -> Html.Attribute Signal
styleTypeRibbon guessedType =
  style "background-image" ("url(assets/images/types/" ++ guessedType ++ ".png)")


getSpecialFormHint : String -> String
getSpecialFormHint identifier =
  if String.contains "-mega" identifier
    || String.contains "-primal" identifier
    || String.contains "-alola" identifier
    || String.contains "-galar" identifier then
     " (forme spéciale)"
  else
     ""
