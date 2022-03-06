module Main exposing (..)

import Browser
import Dict
import Html exposing (Html, div, form, input, text, button, datalist, option, br, span)
import Html.Attributes exposing (class, id, type_, size, placeholder, spellcheck, autofocus, list, value)
import Html.Events exposing (onInput, onSubmit)
import Http
import Random
import Random.List
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
                , pokemonFilteredList = filteredList
                , chosen = pokemon
                , search = ""
                }
              , Cmd.none)
            Nothing ->
              (model, Cmd.none)
        _ ->
          (model, Cmd.none)

    Typed name ->
      case model of
        Chosen data ->
          (Chosen { data | search = name }, Cmd.none)
        _ ->
          (model, Cmd.none)

    Submitted ->
      case model of
        Chosen data ->
          (Chosen { data | search = "" }, Cmd.none)
        _ ->
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
        Chosen data ->
          viewGame data
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
  , div [ class "solution" ]
      [ span [ class ("pokesprite pokemon " ++ gameData.chosen.identifier) ] []
      , text gameData.chosen.species.names.fr
      ]
  , form [ class "pokemon-search", onSubmit Submitted ]
    [ input
      [ type_ "search"
      , value gameData.search
      , size 30
      , placeholder "Entrez le nom d'un Pokémon..."
      , spellcheck False
      , autofocus True
      , onInput Typed
      ] []
    , button [ type_ "submit", class "search" ] []
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
