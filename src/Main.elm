module Main exposing (..)

import Browser
import Html exposing (Html, div, form, input, text, button, datalist, option, br)
import Html.Attributes exposing (class, id, type_, size, placeholder, spellcheck, autofocus, list, value)
import Html.Events exposing (onInput, onSubmit)
import Http
import Types exposing (..)


type Model
  = Ready GameData
  | LoadingPokemonList
  | LoadingPokemonByName PokemonList
  | Failure


type Signal
  = Typed String
  | Submitted
  | ReceivedPokemonList (Result Http.Error PokemonList)
  | ReceivedPokemonByName (Result Http.Error PokemonByName)


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
    Typed name ->
      case model of
        Ready data ->
          (Ready { data | search = name }, Cmd.none)
        _ ->
          (model, Cmd.none)

    Submitted ->
      case model of
        Ready data ->
          (Ready { data | search = "" }, Cmd.none)
        _ ->
          (model, Cmd.none)

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
              (Ready { pokemonList = list, pokemonByName = byName, search = "" }, Cmd.none)
            _ ->
              (Failure, Cmd.none)
        Err _ ->
          (Failure, Cmd.none)


subscriptions : Model -> Sub Signal
subscriptions model =
  Sub.none


view : Model -> Html Signal
view model =
  div [ class "app" ]
    ( case model of
        Ready data ->
          viewGame data
        LoadingPokemonList ->
          viewLoading
        LoadingPokemonByName _ ->
          viewLoading
        Failure ->
          viewFailure
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
