module Game exposing (..)

import Array exposing (Array)
import Dict exposing (Dict)
import Json.Decode as J


type alias GameData =
  { status : GameStatus
  , pokemonTable : PokemonTable
  , pokemonByName : PokemonByName
  , genPokemonByName : PokemonByName
  , pokemonPool : List Pokemon
  , chosen : Pokemon
  , search : String
  , searchResults : PokemonByName
  , guesses : List Pokemon
  , activeGuess : Maybe Pokemon
  }


type GameStatus
  = Guessing
  | Failed
  | Succeeded


type alias PokemonTable =
  Dict String Pokemon


type alias PokemonByName =
  List (String, List Int)


type alias Pokemon =
  { id : Int
  , identifier : String
  , species : PokemonSpecies
  , height : Int
  , weight : Int
  , types : Array String
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


pokemonTableDecoder : J.Decoder PokemonTable
pokemonTableDecoder =
  J.dict
    ( J.map6 Pokemon
        (J.field "id" J.int)
        (J.field "identifier" J.string)
        (J.field "species" pokemonSpeciesDecoder)
        (J.field "height_dm" J.int)
        (J.field "weight_hg" J.int)
        (J.field "types" (J.array J.string))
    )


pokemonSpeciesDecoder : J.Decoder PokemonSpecies
pokemonSpeciesDecoder =
  J.map3 PokemonSpecies
    (J.field "id" J.int)
    ( J.field "names"
      ( J.map2 PokemonNames
        (J.field "fr" J.string)
        (J.field "en" J.string)
      )
    )
    (J.field "generation" J.int)


pokemonByNameDecoder : J.Decoder PokemonByName
pokemonByNameDecoder =
  J.keyValuePairs (J.list J.int)
