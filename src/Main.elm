module Main exposing (..)

import Browser
import Dict exposing (Dict)
import Html exposing (Html, div, form, input, text, button, datalist, option)
import Html.Attributes exposing (class, id, type_, size, placeholder, spellcheck, autofocus, list, value)
import Html.Events exposing (onInput, onSubmit)
import Http
import Json.Decode as J


type alias GameData =
  { pokemonList : PokemonList
  , search : String
  }


type alias PokemonList =
  Dict String Pokemon


type alias Pokemon =
  { id : Int
  , identifier : String
  , species : PokemonSpecies
  , height : Int
  , weight : Int
  , types : List String
  }


type alias PokemonSpecies =
  { id : Int
  , names : PokemonNames
  , generation : Int
  }


type alias PokemonNames =
  { fr : String
  , en : String
  }


type Model
  = Loading
  | Failure
  | Ready GameData


type Signal
  = Typed String
  | Submitted
  | ReceivedPokemonList (Result Http.Error PokemonList)


main =
  Browser.element
    { init = init
    , update = update
    , subscriptions = subscriptions
    , view = view
    }


init : () -> (Model, Cmd Signal)
init _ =
  ( Loading
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
          (Ready { pokemonList = list, search = "" }, Cmd.none)
        Err _ ->
          (Failure, Cmd.none)


subscriptions : Model -> Sub Signal
subscriptions model =
  Sub.none


view : Model -> Html Signal
view model =
  div [ class "app" ]
  [ div [ class "guesses" ]
    [ div [ class "guess" ]
      (List.repeat 4
          ( div [ class "guessline" ]
            [ div [ class "ribbon" ] []
            , div [ class "check" ] []
            ]
          )
      )
    ]
  , form [ class "pokemon-search", onSubmit Submitted ]
    [
      input
      [ type_ "search"
      , size 30
      , placeholder "Entrez le nom d'un Pokémon..."
      , spellcheck False
      , autofocus True
      , onInput Typed
      ] []
    , button [ type_ "submit", class "search" ] []
    ]
  ]


pokemonListDecoder : J.Decoder PokemonList
pokemonListDecoder =
  J.dict
    (J.map6 Pokemon
        (J.field "id" J.int)
        (J.field "identifier" J.string)
        (J.field "species" pokemonSpeciesDecoder)
        (J.field "height_dm" J.int)
        (J.field "weight_hg" J.int)
        (J.field "types" (J.list J.string))
    )


pokemonSpeciesDecoder : J.Decoder PokemonSpecies
pokemonSpeciesDecoder =
  J.map3 PokemonSpecies
    (J.field "id" J.int)
    (J.field "names"
      (J.map2 PokemonNames
        (J.field "fr" J.string)
        (J.field "en" J.string)
        )
      )
    (J.field "generation" J.int)
