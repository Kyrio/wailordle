module Main exposing (..)

import Browser
import Browser.Dom
import Debug exposing (log)
import Dict exposing (Dict)
import Html exposing (Html, div, form, input, text, button, datalist, option, br, span, h1, h2)
import Html.Attributes exposing (class, id, type_, size, placeholder, spellcheck, list, value)
import Html.Events exposing (onInput)
import Http
import Random
import Random.List
import Task
import Types exposing (..)


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
  | Submitted
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
                , searchResults = []
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
            (Chosen { gameData | searchResults = results }, Cmd.none)
        _ ->
          (model, Cmd.none)

    Submitted ->
      case model of
        Chosen gameData ->
          (Chosen gameData, Cmd.none)
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
    [ div [ class "guess" ]
      ( List.repeat 4
          ( div [ class "guessline" ]
            [ div [ class "ribbon" ] []
            , div [ class "check" ] []
            ]
          )
      )
    ]
  , form [ class "pokemon-search" ]
    [ input
      [ type_ "search"
      , id "search-bar"
      , size 30
      , placeholder "Entrez le nom d'un Pokémon..."
      , spellcheck False
      , onInput Typed
      ]
      []
    , div [ class "search-icon" ] []
    , div [ if List.isEmpty gameData.searchResults then (class "results empty") else (class "results") ]
        (List.map (mapSearchResult gameData.pokemonList) gameData.searchResults)
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


filterByName : String -> (String, List Int) -> Bool
filterByName search (name, list) =
  String.startsWith (String.toLower search) (String.toLower name)


mapSearchResult : PokemonList -> (String, List Int) -> Html Signal
mapSearchResult pokemonList (name, list) =
  div [ class "species" ] (List.map (mapVariant pokemonList) list)


mapVariant : PokemonList -> Int -> Html Signal
mapVariant pokemonList variant =
  let
    key = String.fromInt variant
    maybePokemon = Dict.get key pokemonList
    identifier =
      case maybePokemon of
        Just pokemon ->
          pokemon.identifier
        Nothing ->
          "missingno"
    frenchName =
      case maybePokemon of
        Just pokemon ->
          pokemon.species.names.fr
        Nothing ->
          "Missingno"
  in
    div [ class "variant" ]
      [ div [ class "variant-name" ]
        [ h1 [] [ text frenchName ]
        , h2 [] [ text identifier ]
        ]
      , div [ class ("pokesprite pokemon " ++ identifier) ] []
      ]
