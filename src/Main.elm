module Main exposing (main)

import Dict
import Html exposing (Html)
import Html.Attributes
import Html.Events
import Json.Decode
import ModuleGraph
import Ports exposing (File)


main : Program Never Model Msg
main =
    Html.program
        { init = ( NoFile False, Cmd.none )
        , update = update
        , view = view
        , subscriptions = subscriptions
        }


type Model
    = NoFile Bool
    | SelectedFile (Result String ModuleGraph.Model)


type Msg
    = RequestFileInputData
    | ReceiveFileInputData (Maybe File)
    | ModuleGraphMsg ModuleGraph.Msg


fileInputId : String
fileInputId =
    "file-input"


subscriptions : Model -> Sub Msg
subscriptions _ =
    Ports.fileInputData (Tuple.second >> Maybe.map Ports.toFile >> ReceiveFileInputData)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg state =
    case msg of
        RequestFileInputData ->
            ( NoFile True
            , Ports.requestFileInputData fileInputId
            )

        ReceiveFileInputData mFile ->
            ( mFile
                |> Maybe.map (SelectedFile << resultFromFile)
                |> Maybe.withDefault (NoFile False)
            , Cmd.none
            )

        ModuleGraphMsg msg_ ->
            ( case state of
                SelectedFile (Ok moduleGraph) ->
                    SelectedFile (Ok (moduleGraph |> ModuleGraph.update msg_))

                _ ->
                    state
            , Cmd.none
            )


resultFromFile : File -> Result String ModuleGraph.Model
resultFromFile file =
    file.content
        |> Result.andThen
            (Json.Decode.decodeString ModuleGraph.decodeInput)
        |> Result.map
            ModuleGraph.init


view : Model -> Html Msg
view state =
    let
        isAwaitingFile =
            case state of
                NoFile True ->
                    True

                _ ->
                    False
    in
        Html.div
            [ Html.Attributes.style
                [ ( "margin", "40px" )
                , ( "font-family", "Helvetica, Arial, san-serif" )
                , ( "font-size", "12px" )
                ]
            ]
            [ Html.input
                [ Html.Attributes.type_ "file"
                , Html.Attributes.id fileInputId
                , Html.Attributes.disabled isAwaitingFile
                , Html.Attributes.style
                    [ ( "width", "300px" )
                    , ( "margin-bottom", "20px" )
                    , ( "font-size", "12px" )
                    ]
                , Html.Events.on "change" (Json.Decode.succeed RequestFileInputData)
                ]
                []
            , case state of
                SelectedFile (Ok moduleGraph) ->
                    ModuleGraph.view moduleGraph |> Html.map ModuleGraphMsg

                SelectedFile (Err error) ->
                    viewSection
                        [ Html.text error
                        ]

                NoFile _ ->
                    viewSection
                        [ Html.text "To explore package and module dependencies for an Elm project, generate a "
                        , Html.a [ Html.Attributes.href "https://github.com/justinmimbs/elm-module-graph" ] [ Html.text "module-graph.json" ]
                        , Html.text " file, and provide it above."
                        ]
            ]


viewSection : List (Html a) -> Html a
viewSection =
    Html.div
        [ Html.Attributes.style
            [ ( "width", "360px" )
            , ( "margin-top", "20px" )
            ]
        ]
