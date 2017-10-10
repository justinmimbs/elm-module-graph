port module Ports exposing (FileJson, File, toFile, requestFileInputData, fileInputData)


type alias File =
    { name : String
    , content : Result String String
    }


type alias FileJson =
    { name : String
    , error : Maybe String
    , content : Maybe String
    }


type alias Id =
    String


toFile : FileJson -> File
toFile { name, error, content } =
    File
        name
        (content
            |> Maybe.map Ok
            |> Maybe.withDefault (Err (error |> Maybe.withDefault "No error message provided"))
        )


port requestFileInputData : Id -> Cmd a


port fileInputData : (( Id, Maybe FileJson ) -> a) -> Sub a
