module App exposing (..)

import Html exposing (Html, text, div, input, button, form, span)
import Html.Events exposing (onClick, onInput, onSubmit)
import Html.Attributes exposing (class, placeholder, value)
import Json.Encode as Encode
import Json.Decode as Decode
import Json.Decode.Pipeline exposing (decode, required, optional)
import Phoenix.Socket
import Phoenix.Channel
import Phoenix.Push


---- MODEL ----


type alias Model =
    { newMessage : String
    , messages : List ChatMessage
    , phxSocket : Maybe (Phoenix.Socket.Socket Msg)
    , username : String
    }


init : String -> ( Model, Cmd Msg )
init path =
    { newMessage = ""
    , messages = []
    , phxSocket = Nothing
    , username = ""
    }
        ! []


socketServer : String -> String
socketServer username =
    "ws://localhost:4000/socket/websocket?username=" ++ username


initPhxSocket : String -> Phoenix.Socket.Socket Msg
initPhxSocket username =
    socketServer username
        |> Phoenix.Socket.init
        |> Phoenix.Socket.withDebug
        |> Phoenix.Socket.on "new:msg" "room:lobby" ReceiveMessage


type alias ChatMessage =
    { user : String
    , body : String
    }



---- UPDATE ----


type Msg
    = SetNewMessage String
    | JoinChannel
    | PhoenixMsg (Phoenix.Socket.Msg Msg)
    | SendMsg
    | ReceiveMessage Encode.Value
    | SetUsername String
    | ConnectSocket


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SetNewMessage message ->
            { model
                | newMessage = message
            }
                ! []

        JoinChannel ->
            case model.phxSocket of
                Nothing ->
                    model ! []

                Just socket ->
                    let
                        channel =
                            Phoenix.Channel.init "room:lobby"

                        ( phxSocket, phxCmd ) =
                            Phoenix.Socket.join channel socket
                    in
                        { model
                            | phxSocket = Just phxSocket
                        }
                            ! [ Cmd.map PhoenixMsg phxCmd ]

        PhoenixMsg msg ->
            case model.phxSocket of
                Nothing ->
                    model ! []

                Just socket ->
                    let
                        ( phxSocket, phxCmd ) =
                            Phoenix.Socket.update msg socket
                    in
                        { model
                            | phxSocket = Just phxSocket
                        }
                            ! [ Cmd.map PhoenixMsg phxCmd ]

        SendMsg ->
            case model.phxSocket of
                Nothing ->
                    model ! []

                Just socket ->
                    let
                        payload =
                            encodeMessage model.newMessage

                        push =
                            Phoenix.Push.init "new:msg" "room:lobby"
                                |> Phoenix.Push.withPayload payload

                        ( phxSocket, phxCmd ) =
                            Phoenix.Socket.push push socket
                    in
                        { model
                            | newMessage = ""
                            , phxSocket = Just phxSocket
                        }
                            ! [ Cmd.map PhoenixMsg phxCmd ]

        ReceiveMessage encodedMessage ->
            case Decode.decodeValue decodeMessage encodedMessage of
                Ok chatMessage ->
                    { model
                        | messages = model.messages ++ [ chatMessage ]
                    }
                        ! []

                Err error ->
                    model ! []

        SetUsername username ->
            { model
                | username = username
            }
                ! []

        ConnectSocket ->
            { model
                | phxSocket = Just (initPhxSocket model.username)
            }
                ! []


decodeMessage : Decode.Decoder ChatMessage
decodeMessage =
    decode ChatMessage
        |> optional "user" Decode.string "anonymous"
        |> required "body" Decode.string


encodeMessage : String -> Encode.Value
encodeMessage message =
    Encode.object
        [ ( "body", Encode.string message ) ]



---- VIEW ----


view : Model -> Html Msg
view model =
    case model.phxSocket of
        Nothing ->
            setUsernameView

        Just socket ->
            chatInterfaceView model


setUsernameView : Html Msg
setUsernameView =
    form [ onSubmit ConnectSocket ]
        [ input [ placeholder "Enter a username...", onInput SetUsername ] [] ]


chatInterfaceView : Model -> Html Msg
chatInterfaceView model =
    div []
        [ lobbyView
        , messageListView model.messages
        , inputView model.newMessage
        ]


lobbyView : Html Msg
lobbyView =
    button [ onClick JoinChannel ] [ text "Join lobby" ]


inputView : String -> Html Msg
inputView currentInput =
    form [ onSubmit SendMsg ]
        [ input [ placeholder "Message...", onInput SetNewMessage, value currentInput ] [] ]


messageListView : List ChatMessage -> Html Msg
messageListView chatMessages =
    div []
        (List.map viewMessage chatMessages)


viewMessage : ChatMessage -> Html Msg
viewMessage message =
    div []
        [ span [] [ text (message.user ++ ": ") ]
        , span [] [ text message.body ]
        ]



---- PROGRAM ----


main : Program String Model Msg
main =
    Html.programWithFlags
        { view = view
        , init = init
        , update = update
        , subscriptions = subscriptions
        }


subscriptions : Model -> Sub Msg
subscriptions model =
    case model.phxSocket of
        Nothing ->
            Sub.none

        Just socket ->
            Phoenix.Socket.listen socket PhoenixMsg
