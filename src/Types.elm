module Types exposing (GameData, PokemonList, PokemonByName, Pokemon, PokemonSpecies, PokemonNames, pokemonListDecoder, pokemonByNameDecoder)

import Dict exposing (Dict)
import Json.Decode as J


type alias GameData =
  { pokemonList : PokemonList
  , pokemonByName : PokemonByName
  , search : String
  }


type alias PokemonList =
  Dict String Pokemon


type alias PokemonByName =
  Dict String (List Int)


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


pokemonListDecoder : J.Decoder PokemonList
pokemonListDecoder =
  J.dict
    ( J.map6 Pokemon
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
    ( J.field "names"
      ( J.map2 PokemonNames
        (J.field "fr" J.string)
        (J.field "en" J.string)
      )
    )
    (J.field "generation" J.int)


pokemonByNameDecoder : J.Decoder PokemonByName
pokemonByNameDecoder =
  J.dict (J.list J.int)
