import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode as Decode
import Json.Encode as Encode

main : Program Never Model Msg
main =
  Html.program
    { init = start
    , view = view
    , update = update
    , subscriptions = subscriptions
    }

url : String -> String
url action = "http://localhost:9000/member/" ++ action

-- MODEL

type alias Member =
  { id : Int
  , name : String
  , email : String
  }

type alias Model =
  { count : Int
  , message : String
  , member : Member
  }

start : (Model, Cmd Msg)
start =
  ( Model 0 "No message" (Member 99 "PeterBoss" "cph-pt75@cphbusiness.dk")
  , Cmd.none
  )

-- UPDATE

type Msg
  = GetMemberCount
  | MemberCountReceived (Result Http.Error Int)
  | GetMember
  | MemberReceived (Result Http.Error Member)
  | UpdateID String
  | UpdateName String
  | UpdateEmail String
  | PostMember
  | MemberPosted (Result Http.Error Member)

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    GetMemberCount ->
      (model, getMemberCount)

    MemberCountReceived (Ok newCount) ->
      ( { model | count = newCount }, Cmd.none)

    MemberCountReceived (Err error) ->
      ( { model | message = toString error }, Cmd.none)

    GetMember ->
      (model, getMember)

    MemberReceived (Ok newMember)->
      ( { model | member = newMember}, Cmd.none )

    MemberReceived (Err error) ->
      ( { model | message = toString error }, Cmd.none )

    UpdateID newID->
      case String.toInt newID of
        Ok id ->
          ( { model | member = (Member id model.member.name model.member.email)}, Cmd.none )
        Err _ ->
          ( { model | member = (Member 0 model.member.name model.member.email),
                      message = "Invalid ID"}, Cmd.none)

    UpdateName name ->
      let
        member = model.member
      in
        ( { model | member = { member | name = name } }, Cmd.none)

    UpdateEmail email ->
      let
        member = model.member
      in
       ( { model | member = { member | email = email } }, Cmd.none)

    PostMember ->
      (model, postMember model.member)

    MemberPosted (Ok newMember) ->
      ( { model | member = newMember}, getMemberCount )

    MemberPosted (Err error) ->
      ( { model | message = toString error }, Cmd.none )
-- VIEW

view : Model -> Html Msg
view model =
  div []
    [ h2 [] [text ("Member Count = " ++ toString model.count) ]
    , button [ onClick GetMemberCount ] [ text "Update Member Count" ]
    , button [ onClick GetMember ] [ text "Get Member" ]
    , button [ onClick PostMember ] [ text "Post Member" ]
    , hr [] []
    , input [type_ "text", value (toString model.member.id), onInput UpdateID] []
    , input [type_ "text", value model.member.name, onInput UpdateName] []
    , input [type_ "text", value model.member.email, onInput UpdateEmail] []
    , hr [] []
    , text model.message
    ]

-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model = Sub.none

-- HTTP

getMemberCount : Cmd Msg
getMemberCount =
    Http.send MemberCountReceived (Http.get (url "count") Decode.int)

getMember : Cmd Msg
getMember =
  Http.send MemberReceived (Http.get (url "1") decodeMember)

decodeMember : Decode.Decoder Member
decodeMember =
  Decode.map3 Member
    (Decode.field "id" Decode.int)
    (Decode.field "name" Decode.string)
    (Decode.field "email" Decode.string)

encodeMember : Member -> Encode.Value
encodeMember member =
  Encode.object
    [ ("id", Encode.int member.id)
    , ("name", Encode.string member.name)
    , ("email", Encode.string member.email)
    ]

memberJsonBody : Member -> Http.Body
memberJsonBody member =
  Http.jsonBody <| encodeMember member

postMember : Member -> Cmd Msg
postMember member =
  Http.send MemberPosted (Http.post (url "") (memberJsonBody member) decodeMember)
