module Register exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onSubmit,onInput)
import String exposing (contains,length,any)
import Char exposing (isDigit, isUpper, isLower)


type alias SignUpForm =
  { email : String
  , password : String
  , confirmPassword : String
  }

type Problem
  = EmailInvalid String
  | PasswordInvalid
  | PasswordsDoNotMatch

type alias Model =
  { form     : SignUpForm 
  , problems : List Problem
  }

type Msg
  = FormSubmitted
  | EmailEntered String
  | PasswordEntered String
  | ConfirmPasswordEntered String


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    FormSubmitted ->
      ( model , Cmd.none )
    EmailEntered email ->
      updateForm (\form -> { form | email = email }) model
    PasswordEntered password ->
      updateForm (\form -> { form | password = password }) model
    ConfirmPasswordEntered confirmPassword ->
      updateForm (\form -> { form | confirmPassword = confirmPassword }) model

updateForm : (SignUpForm -> SignUpForm) -> Model -> (Model,Cmd Msg)
updateForm func model =
  ({ model | form = func model.form }, Cmd.none)

-- validate : SignUpForm -> Result (List Problem) SignUpForm
-- validate form =

validateEmail : String -> Result Problem String
validateEmail email =
  if email |> contains "@" then
    Ok email
  else
    Err (EmailInvalid email)

validatePassword : String -> Result Problem String
validatePassword password =
  let
    hasUpperCase     = password |> any isUpper
    hasLowerCaase    = password |> any isLower
    hasSpecial       = password |> containsSpecial
    contiansDigit    = password |> any isDigit
    pwLength         = password |> length
    has8Chars        = pwLength >= 8
    isLessThan18Char = pwLength <= 18
  in
  
  if has8Chars && isLessThan18Char && hasUpperCase && hasLowerCaase && hasSpecial && contiansDigit then
    Ok password
  else
    Err PasswordInvalid

containsSpecial : String -> Bool
containsSpecial item =
  (item |> contains "!") || 
  (item |> contains "@") ||
  (item |> contains "#") ||
  (item |> contains "$") ||
  (item |> contains "%") ||
  (item |> contains "^") ||
  (item |> contains "&") ||
  (item |> contains "*") ||
  (item |> contains "(") ||
  (item |> contains ")") ||
  (item |> contains "_") ||
  (item |> contains "-") ||
  (item |> contains "+") ||
  (item |> contains "~") ||
  (item |> contains "<") ||
  (item |> contains ">") ||
  (item |> contains ",") ||
  (item |> contains ".") ||
  (item |> contains "?") ||
  (item |> contains ";") ||
  (item |> contains ":") ||
  (item |> contains "|")
  

formView : SignUpForm -> Html Msg
formView form = 
  Html.form [ onSubmit FormSubmitted ]
    [ fieldset [ class "form-group" ]
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
    , fieldset [ class "form-group" ]
      [ input
        [ class "form-control"
        , placeholder "Confirm Password"
        , onInput ConfirmPasswordEntered
        , value form.confirmPassword
        ] []
      ]
    , button [class "btn btn-primary"] [ text "Sign Up" ]
    ]