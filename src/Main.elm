module Main exposing (..)

import Array
import Browser
import Browser.Dom
import Debug exposing (log)
import Dict
import Html exposing (Html, div, form, input, text, br, span, h1, h2, a)
import Html.Attributes exposing (class, id, type_, size, placeholder, spellcheck, value, href, style)
import Html.Events exposing (onInput, onClick)
import Http
import Random
import Random.List
import Task

import Game exposing (..)


type Model
  = LoadingPokemonList
  | LoadingPokemonByName PokemonList
  | Failure
  | Ready (PokemonList, PokemonByName, List Pokemon)
  | Chosen GameData


type Signal
  = ReceivedPokemonList (Result Http.Error PokemonList)
  | ReceivedPokemonByName (Result Http.Error PokemonByName)
  | ChosePokemon (Maybe Pokemon, List Pokemon)
  | Typed String
  | Submitted Pokemon
  | NoOp


main =
  Browser.element
    { init = init
    , update = update
    , subscriptions = subscriptions
    , view = view
    }


init : () -> (Model, Cmd Signal)
init _ =
  ( LoadingPokemonList
  , Http.get
      { url = "/assets/json/pokemon_list.json"
      , expect = Http.expectJson ReceivedPokemonList pokemonListDecoder
      }
  )


update : Signal -> Model -> (Model, Cmd Signal)
update signal model =
  case signal of
    ReceivedPokemonList result ->
      case result of
        Ok list ->
          ( LoadingPokemonByName list
          , Http.get
              { url = "/assets/json/pokemon_by_french_name.json"
              , expect = Http.expectJson ReceivedPokemonByName pokemonByNameDecoder
              }
          )
        Err _ ->
          (Failure, Cmd.none)

    ReceivedPokemonByName result ->
      case result of
        Ok byName ->
          case model of
            LoadingPokemonByName list ->
              let
                -- Exclude Crown Tundra Pokemon
                filteredList = Dict.values list
              in
                ( Ready (list, byName, filteredList)
                , Random.generate ChosePokemon (Random.List.choose filteredList)
                )
            _ ->
              (model, Cmd.none)
        Err _ ->
          (Failure, Cmd.none)

    ChosePokemon (maybe, rest) ->
      case model of
        Ready (list, byName, filteredList) ->
          case maybe of
            Just pokemon ->
              (Chosen
                { pokemonList = list
                , pokemonByName = byName
                , pokemonPool = rest
                , chosen = pokemon
                , search = ""
                , searchResults = []
                , guess = Thinking
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
                List.filter (filterByName search) gameData.pokemonByName
          in
            (Chosen { gameData | search = search, searchResults = results }, Cmd.none)
        _ ->
          (model, Cmd.none)

    Submitted pokemon ->
      case model of
        Chosen gameData ->
          ( Chosen
              { gameData
              | guess =
                  if pokemon.identifier == gameData.chosen.identifier then
                    GuessedRight
                  else
                    GuessedWrong pokemon
              , search = ""
              , searchResults = []
              }
          , Task.attempt (\_ -> NoOp) (Browser.Dom.focus "search-bar")
          )
        _ ->
          (model, Cmd.none)

    NoOp ->
      (model, Cmd.none)


subscriptions : Model -> Sub Signal
subscriptions model =
  Sub.none


view : Model -> Html Signal
view model =
  div [ class "app" ]
    ( case model of
        LoadingPokemonList ->
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
  [ div [ class "guesses" ]
    [ div [ class "guess" ] (viewGuess gameData.chosen gameData.guess)
    ]
  , form [ class "pokemon-search" ]
    [ input
      [ type_ "search"
      , id "search-bar"
      , size 30
      , placeholder "Entrez le nom d'un Pokémon..."
      , spellcheck False
      , onInput Typed
      , value gameData.search
      ]
      []
    , div [ class "search-icon" ] []
    , div [ if List.isEmpty gameData.searchResults then (class "results empty") else (class "results") ]
        (List.map (mapSearchResult gameData.pokemonList) gameData.searchResults)
    ]
  ]


viewGuess : Pokemon -> Guess -> List (Html Signal)
viewGuess chosen guess =
  case guess of
    Thinking ->
      List.repeat 4
        ( div [ class "guessline" ]
          [ div [ class "ribbon" ] []
          , div [ class "check" ] []
          ]
        )

    GuessedRight ->
      viewComparison chosen chosen

    GuessedWrong guessed ->
      viewComparison chosen guessed


viewComparison : Pokemon -> Pokemon -> List (Html Signal)
viewComparison chosen guessed =
  let
    chosenTypeA = Maybe.withDefault "none" (Array.get 0 chosen.types)
    chosenTypeB = Maybe.withDefault "none" (Array.get 1 chosen.types)
    guessedTypeA = Maybe.withDefault "none" (Array.get 0 guessed.types)
    guessedTypeB = Maybe.withDefault "none" (Array.get 1 guessed.types)

    checkTypeA =
      if guessedTypeA == chosenTypeA then
        div [ class "check correct" ] []
      else if guessedTypeA == chosenTypeB then
        div [ class "check swap" ] []
      else
        div [ class "check wrong" ] []

    checkTypeB =
      if guessedTypeB == chosenTypeB then
        div [ class "check correct" ] []
      else if guessedTypeB == chosenTypeA then
        div [ class "check swap" ] []
      else
        div [ class "check wrong" ] []

    heightTest =
      if guessed.height < chosen.height then
        [ div [ class "ribbon too-short" ] []
        , div [ class "check too-low" ] []
        ]
      else if guessed.height > chosen.height then
        [ div [ class "ribbon too-tall" ] []
        , div [ class "check too-high" ] []
        ]
      else
        [ div [ class "ribbon correct-height" ] []
        , div [ class "check correct" ] []
        ]

    weightTest =
      if guessed.weight < chosen.weight then
        [ div [ class "ribbon too-light" ] []
        , div [ class "check too-low" ] []
        ]
      else if guessed.weight > chosen.weight then
        [ div [ class "ribbon too-heavy" ] []
        , div [ class "check too-high" ] []
        ]
      else
        [ div [ class "ribbon correct-weight" ] []
        , div [ class "check correct" ] []
        ]
  in
    [ div [ class "guessline" ]
        [ div [ class "ribbon", style "background-image" ("url(assets/images/types/" ++ guessedTypeA ++ ".png)") ] []
        , checkTypeA
        ]
    , div [ class "guessline" ]
        [ div [ class "ribbon", style "background-image" ("url(assets/images/types/" ++ guessedTypeB ++ ".png)") ] []
        , checkTypeB
        ]
    , div [ class "guessline" ] heightTest
    , div [ class "guessline" ] weightTest
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


mapSearchResult : PokemonList -> (String, List Int) -> Html Signal
mapSearchResult pokemonList (name, list) =
  div [ class "species" ] (List.map (mapVariant pokemonList) list)


mapVariant : PokemonList -> Int -> Html Signal
mapVariant pokemonList variant =
  let
    key = String.fromInt variant
  in
    case Dict.get key pokemonList of
      Nothing ->
        div [ class "alert" ] [ text "Missingno" ]
      Just pokemon ->
        a [ href "#", class "variant", onClick (Submitted pokemon) ]
          [ div [ class "variant-name" ]
            [ h1 [] [ text pokemon.species.names.fr ]
            , h2 [] [ text pokemon.identifier ]
            ]
          , div [ class ("pokesprite pokemon " ++ pokemon.identifier) ] []
          ]


filterByName : String -> (String, List Int) -> Bool
filterByName search (name, list) =
  String.startsWith (String.toLower search) (String.toLower name)
