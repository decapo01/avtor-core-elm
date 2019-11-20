module Avtor exposing (..)

import Http
import Json.Decode as D
import Json.Encode as E

type alias User = 
  { id    : String
  , email : String 
  }

type alias RegisterDto =
  { email    : String
  , password : String
  , confirm  : String
  }

type alias LoginDto =
  { email    : String
  , password : String
  }

type alias Model =
  { location   : String
  , apiVersion : String
  }


type Msg
  = RegistrationPosted (Result Http.Error String)
  | DoneGot (Result Http.Error String)


mapRegisterDtoToJsonBody : RegisterDto -> Http.Body
mapRegisterDtoToJsonBody dto =
  E.object
    [ ("email"          , E.string dto.email    )
    , ("password"       , E.string dto.password )
    , ("confirmPassword", E.string dto.confirm  )
    ]
  |> Http.jsonBody

postRegisterDto : Model -> RegisterDto -> Cmd Msg
postRegisterDto model dto =
  Http.post
    { url    = model.location ++ "/api/" ++ model.apiVersion ++ "/avtor/register"
    , body   = mapRegisterDtoToJsonBody dto
    , expect = Http.expectString RegistrationPosted 
    }

getBlah : Cmd Msg
getBlah =
  Http.get
    { url    = ""
    , expect = Http.expectString DoneGot
    }