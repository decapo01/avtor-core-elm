module Avtor exposing (..)

import Browser exposing (UrlRequest(..))
import Browser.Navigation as Nav
import Http
import Json.Decode as D
import Json.Encode as E
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)
import Url exposing (Url)
import Url.Parser as Url exposing (Parser,(</>))


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
  { location    : String
  , apiVersion  : String
  , navKey      : Nav.Key
  , page        : Page
  , registerDto : RegisterDto
  , loginDto    : LoginDto
  }

type Msg
  = RegistrationPosted (Result Http.Error String)
  | DoneGot (Result Http.Error String)
  | UrlChange Url.Url
  | LinkClicked UrlRequest
  | LoginMsg String
  | EmailMsg String
  | PasswordMsg String
  | ConfirmPasswordMsg String
  | LoginEmailEntered String
  | LoginPasswordEntered String

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    LinkClicked urlRequest ->
      case urlRequest of
        Internal url ->
            ( model
            , Nav.pushUrl model.navKey (Url.toString url)
            )
        External url ->
          ( model
          , Nav.load url
          )
    UrlChange url ->
      ( { model | page = urlToPage url }
      , Cmd.none
      )
    LoginEmailEntered
    _ -> ( model, Cmd.none )

type MapType
  = Register { dto : RegisterDto }
  | Login    { dto : LoginDto    }

type Page
  = IndexPage
  | RegisterPage
  | LoginPage

type Routes
  = HomeRoute
  | RegisterRoute
  | LoginRoute

viewPage : Page -> Model -> Html Msg
viewPage page model =
  case page of
    IndexPage    -> indexView
    RegisterPage -> registerView model
    LoginPage    -> loginView model


urlParser : Parser (Page -> a) a
urlParser =
  Url.oneOf
  [ Url.map IndexPage Url.top
  , Url.map RegisterPage (Url.s "register")
  , Url.map LoginPage (Url.s "login")
  ]

urlToPage : Url -> Page
urlToPage url =
  url
  |> Url.parse urlParser
  |> Maybe.withDefault IndexPage

mapDto : MapType -> Http.Body
mapDto mapType =
  case mapType of
    Register item ->
      E.object
        [ ("email"          , E.string item.dto.email    )
        , ("password"       , E.string item.dto.password )
        , ("confirmPassword", E.string item.dto.confirm  )
        ]
      |> Http.jsonBody
    Login item ->
      E.object
        [ ("email"   , E.string item.dto.email    )
        , ("password", E.string item.dto.password )
        ]
      |> Http.jsonBody

postDto : Model -> MapType -> Cmd Msg
postDto model mType =
  Http.post
    { url    = model.location ++ "/api/" ++ model.apiVersion ++ "/avtor/login"
    , body   = mapDto mType
    , expect = Http.expectString RegistrationPosted 
    }

indexView : Html msg
indexView =
  div [] 
  [ h2 [] [text "Home"]
  , ul []
    [ li [] [ a [ href "/login"] [ text "Login" ] ] 
    , li [] [ a [ href "/register" ] [ text "Register" ] ]
    ]
  ]

viewInput : String -> String -> String -> (String -> Msg) -> Html Msg
viewInput iType placeholder_ value_ toMsg =
  input [ type_ iType, placeholder placeholder_, value value_, onInput toMsg ] []

loginView : Model -> Html Msg
loginView model =
  Html.form [] 
    [ div [] [ label [] [ text "Email" ] ]
    , div [] [ viewInput "text" "Email" model.loginDto.email LoginEmailEntered ]
    , div [] [ label [] [ text "Password" ] ]
    , div [] [ viewInput "text" ]
    , div [] [ input [ type_ "button", onInput LoginMsg, value "Login"] [text "Login"] ]
    ]

registerView : Model -> Html Msg
registerView _ =
  Html.form [] 
  [ div [] [ label [] [ text "Email" ] ]
  , div [] [ viewInput "text" ]
  , div [] [ label [] [ text "Password"] ]
  , div [] [ viewInput "text" ]
  , div [] [ label [] [ text "Confirm Password"]]
  , div [] [ viewInput "text" ]
  , div [] [ viewInput "submit" ]
  ]

init : () -> Url.Url -> Nav.Key -> (Model, Cmd Msg)
init flags url key =
  ( { navKey = key
    , page   = urlToPage url
    , location = ""
    , apiVersion = "1"
    }
  , Cmd.none
  )

view : Model -> Browser.Document Msg
view model =
  { title = "app"
  , body =
      [ h1 [] [ text "Awesome App" ] 
      , case model.page of
          IndexPage -> indexView
          RegisterPage -> registerView model
          LoginPage    -> loginView model
      ]
  }

main =
  Browser.application 
  { init   = init
  , update = update
  , view   = view
  , subscriptions = always Sub.none
  , onUrlRequest = LinkClicked
  , onUrlChange = UrlChange
  }