module Main exposing (..)

import Browser
import Html exposing (Html, div, form, input, text, button, datalist, option)
import Html.Attributes exposing (class, id, type_, size, placeholder, spellcheck, autofocus, list, value)
import Html.Events exposing (onInput, onSubmit)


type alias Model =
  String


type Signal
  = Typed String
  | Submitted


main =
  Browser.sandbox { init = init, update = update, view = view }


init : Model
init =
  ""


update : Signal -> Model -> Model
update signal model =
  case signal of
    Typed name ->
      name
    Submitted ->
      ""


view : Model -> Html Signal
view model =
  div [ class "app" ]
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
    [
      input
      [ type_ "search"
      , value model
      , size 30
      --, list "pokemon"
      , placeholder "Entrez le nom d'un Pokémon..."
      , spellcheck False
      , autofocus True
      , onInput Typed
      ]
      []
    , datalist [ id "pokemon" ]
      [ option [ value "Bulbizarre" ] []
      ]
    , button [ type_ "submit", class "search" ] []
    ]
  ]
