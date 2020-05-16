module Elm.Processing.Documentation exposing (postProcess)

import Dict exposing (Dict)
import Elm.Inspector as Inspector exposing (Order(..), defaultConfig)
import Elm.Syntax.Comments exposing (Comment)
import Elm.Syntax.Declaration exposing (Declaration(..))
import Elm.Syntax.Documentation exposing (..)
import Elm.Syntax.Expression exposing (..)
import Elm.Syntax.File exposing (File)
import Elm.Syntax.Node exposing (Node(..))
import Elm.Syntax.Range exposing (Range)
import Elm.Syntax.Type exposing (Type)
import Elm.Syntax.TypeAlias exposing (TypeAlias)


postProcess : File -> File
postProcess file =
    let
        ranges =
            buildDict file
    in
    Inspector.inspect
        { defaultConfig
            | onFunction = Post (onFunction ranges)
            , onTypeAlias = Post (onTypeAlias ranges)
            , onType = Post (onType ranges)
        }
        file
        file


onType : Dict Int (Node Comment) -> Node Type -> File -> File
onType dict (Node r customType) file =
    case Dict.get r.start.row dict of
        Just ((Node docRange docString) as doc) ->
            { file
                | comments =
                    file.comments
                        |> List.filter ((/=) doc)
                , declarations =
                    List.map
                        (replaceDeclaration
                            (Node r (CustomTypeDeclaration <| { customType | documentation = Just (Node docRange docString) }))
                        )
                        file.declarations
            }

        Nothing ->
            file


onTypeAlias : Dict Int (Node Comment) -> Node TypeAlias -> File -> File
onTypeAlias dict (Node r typeAlias) file =
    case Dict.get r.start.row dict of
        Just ((Node docRange docString) as doc) ->
            { file
                | comments =
                    file.comments
                        |> List.filter ((/=) doc)
                , declarations =
                    List.map
                        (replaceDeclaration
                            (Node r
                                (AliasDeclaration
                                    { typeAlias
                                        | documentation =
                                            Just (Node docRange docString)
                                    }
                                )
                            )
                        )
                        file.declarations
            }

        Nothing ->
            file


onFunction : Dict Int (Node Comment) -> Node Function -> File -> File
onFunction dict (Node functionRange function) file =
    case Dict.get functionRange.start.row dict of
        Just ((Node docRange docString) as doc) ->
            { file
                | comments =
                    file.comments
                        |> List.filter ((/=) doc)
                , declarations =
                    List.map
                        (replaceDeclaration
                            (Node functionRange (FunctionDeclaration { function | documentation = Just (Node docRange docString) }))
                        )
                        file.declarations
            }

        Nothing ->
            file


replaceDeclaration : Node Declaration -> Node Declaration -> Node Declaration
replaceDeclaration (Node r1 new) (Node r2 old) =
    Node r2
        (if r1 == r2 then
            new

         else
            old
        )


buildDict : File -> Dict Int (Node Comment)
buildDict file =
    file.comments
        |> List.foldl
            (\((Node range comment) as node) acc ->
                if String.startsWith "{-|" comment then
                    Dict.insert (range.end.row + 1) node acc

                else
                    acc
            )
            Dict.empty
