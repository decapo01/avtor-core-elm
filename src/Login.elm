module Login exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onSubmit,onInput)

type alias LoginForm =
  { email    : String
  , password : String
  }

type Problem
  = EmailInvalid String

type alias Model =
  { form     : LoginForm
  , problems : List Problem
  }

type Msg
  = FormSubmitted
  | EmailEntered String
  | PasswordEntered String

update : Msg -> Model -> ( Model , Cmd Msg )
update msg model =
  case msg of
    FormSubmitted -> 
      ( model , Cmd.none )
    EmailEntered email -> 
      updateForm (\form -> { form | email = email }) model
    PasswordEntered password ->
      updateForm (\form -> { form | password = password }) model

updateForm : (LoginForm -> LoginForm) -> Model -> ( Model, Cmd Msg )
updateForm f model =
  ({ model | form = f model.form }, Cmd.none )

validate : Form -> Result (List Problem) Form


formView : LoginForm -> Html Msg
formView form =
  Html.form [ onSubmit FormSubmitted ]
    [ fieldset [ class "form-group"]
        [ input 
          [ class "form-control"
          , placeholder "Email"
          , onInput EmailEntered
          , value form.email 
          ] []
        ]
    , fieldset [ class "form-group" ]
        [ input 
          [ class "form-control"
          , placeholder "Password"
          , onInput PasswordEntered
          , value form.password 
          ] []
        ]
    , button [ class "btn btn-primary"]
      [ text "Sign In"]
    ]