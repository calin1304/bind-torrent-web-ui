import Debug exposing (todo, log, toString)

import Browser
import Time
import Http
import Task

import Html.Events exposing (onClick)
import Json.Decode exposing (Decoder, dict, string, field, int)
import File exposing (File)
import Html exposing (Html)

import Html as Html
import Html.Attributes as HtmlAttr
import File.Select as Select
import Json.Decode as Decode
import Filesize as Filesize

type alias Torrent = 
    { name : String
    , infoHash : String
    , size : Int
    , dateAdded : String
    }

type alias TorrentStatus = 
    { downloaded : Int
    , downloadSpeed : Int
    }

type alias Model =
    { torrent : Maybe Torrent
    , torrentStatus : Maybe TorrentStatus
    , showSideBar : Bool
    }

type Msg = Tick Time.Posix
         | TorrentRequested
         | TorrentSelected File
         | TorrentUploaded (Result Http.Error Torrent)
         | TorrentStatusLoaded (Result Http.Error TorrentStatus)
         | CancelTorrent
         | TorrentCanceled (Result Http.Error ())
         | ToggleSideBar

main = Browser.element { init = testInit
                       , view = view
                       , update = update
                       , subscriptions = subscriptions 
                       }

init : () -> (Model, Cmd Msg)
init _ = (emptyModel, Cmd.none)

testInit : () -> (Model, Cmd Msg)
testInit _ = (testModel, Cmd.none)

emptyModel : Model
emptyModel =
    { torrent = Nothing
    , torrentStatus = Nothing
    , showSideBar = False
    }

testModel : Model
testModel =
    { torrent =
        Just { name = "elementaryos-5.0-stable.20181016.iso"
             , infoHash = "3e78dd23c4fd014e0a81805d58092d78dd1f9f1b"
             , size = 1400000000
             , dateAdded = "03-08-2019"
             }
    , torrentStatus = Just {downloaded = 32, downloadSpeed = 23000}
    , showSideBar = False
    }

update : Msg -> Model -> (Model, Cmd Msg)
update msg model = 
    case msg of
        Tick t -> (model, getTorrentStatus)
        TorrentRequested -> 
            ( model
            , Select.file ["application/x-bittorrent"] TorrentSelected
            )
        TorrentSelected file ->
            ( model
            , Http.post
                { url = "/loadTorrent"
                , body = Http.fileBody file
                , expect = Http.expectJson TorrentUploaded torrentDecode
                }
            )
        TorrentUploaded result ->
            case result of
                Ok s -> ({model | torrent = Just s}, Cmd.none)
                Err e -> todo <| toString e
        TorrentStatusLoaded result ->
            case result of
                Ok s -> 
                    ({model | torrentStatus = Just s}, Cmd.none)
                Err e -> todo <| toString e
        CancelTorrent -> 
                ( emptyModel
                , Http.post
                    { url = "/cancel"
                    , body = Http.emptyBody
                    , expect = Http.expectWhatever TorrentCanceled
                    }
                )
        TorrentCanceled result ->
                (model, Cmd.none)
        ToggleSideBar -> ( { model | showSideBar = not model.showSideBar }, Cmd.none )

getTorrentStatus : Cmd Msg
getTorrentStatus = 
    Http.get
        { url = "/status"
        , expect = Http.expectJson TorrentStatusLoaded torrentStatusDecode
        }

torrentDecode : Decoder Torrent
torrentDecode = 
    Decode.map4 Torrent 
        (field "tiName" string) 
        (field "tiHash" string) 
        (field "tiSize" int)
        (field "tiDateAdded" string)

torrentStatusDecode : Decoder TorrentStatus
torrentStatusDecode = Decode.map2 TorrentStatus (field "tsDownloaded" int) (field "tsDownloadSpeed" int)

subscriptions : Model -> Sub Msg
subscriptions model = 
    case model.torrent of 
        Just s -> Time.every 1000 Tick
        Nothing -> Sub.none 

view : Model -> Html Msg
view model = 
    let rightContainer = 
            Html.div ([ HtmlAttr.id "right-container"] ++ rightContainerStyle)
                [ viewTorrentDetails model ]
        rightContainerStyle = 
            let hiddenStyle = 
                    [ HtmlAttr.style "max-width" "0"
                    , HtmlAttr.style "opacity" "0"
                    ]
                defaultStyle =
                    [ HtmlAttr.style "max-width" "1000px"
                    , HtmlAttr.style "opacity" "1"
                    ]
             in if not model.showSideBar then hiddenStyle else defaultStyle
     in Html.div [ HtmlAttr.id "container" ]
            [ Html.div [ HtmlAttr.id "left-container" ]
                [ -- viewTitleBar 
                viewToolbar 
                -- , viewContent model
                , viewTorrentList model
                , viewFooter model
                ]
            , rightContainer
            ]

viewTitleBar = 
    Html.div [ HtmlAttr.id "titleBar" ] 
        [ Html.div [ HtmlAttr.id "toolbar-logo" ]
            [ Html.img [ HtmlAttr.src "img/logo.svg" ] []
            , Html.h3 [] [ Html.text "Torrent Client" ]
            ]
        , button [ onClick ToggleSideBar ] [ viewIcon "bars" 2 ]
        ]

viewToolbar = 
    Html.div [ HtmlAttr.id "toolbar" ]
        [ viewTitleBar  
        , Html.div [ HtmlAttr.id "toolbar-buttons" ]
                [ button [ onClick TorrentRequested ] 
                [ viewIcon "plus" 1
                , Html.text "Adaugă" 
                ]
            , button [ onClick CancelTorrent ] 
                [ viewIcon "stop" 1 
                , Html.text "Stop" 
                ]
            , button [ ] 
                [ viewIcon "sliders" 1
                , Html.text "Opțiuni" 
                ]
            ]
        ]

viewContent model =
    Html.div [ HtmlAttr.id "content" ]
        [ viewTorrentList model
        -- , viewTorrentDetails model
        ]

viewFooter model = 
    let icon name url = 
            Html.a [ HtmlAttr.id "icon", HtmlAttr.href url ] 
                [ Html.i [HtmlAttr.class (String.concat ["fa fa-", name, " fa-2x"])] [] ]
        socialIcons = Html.div [ HtmlAttr.id "icons" ] 
                        [ icon "envelope" "mailto:calin.nicolau@yahoo.com"
                        , icon "github" "#", icon "instagram" "#", icon "facebook" "#"]
        link t url = Html.a [ HtmlAttr.href url ] [ Html.span [] [ Html.text t ] ]
        linkList = 
            List.intersperse (Html.span [] [ Html.text " / "]) 
                [ link "Politică de confidențialitate" "#", link "Ajutor" "#" ]
            |> Html.div []
     in Html.footer []
         [ Html.p [] [ Html.text "2019 - Călin Nicolau"]
         --, linkList 
         , socialIcons
         ]

viewIcon name size = Html.i [ HtmlAttr.class (String.concat ["fa fa-", name, " fa-", toString size, "x"]) ] []

viewTorrentList : Model -> Html Msg
viewTorrentList model = 
    let v = case model.torrent of
                Nothing -> Html.div [] []
                Just t -> 
                    case model.torrentStatus of
                        Nothing -> Html.div [] []
                        Just    s -> viewTorrent t s
     in Html.div [ HtmlAttr.id "torrentList" ] [ v ]

viewTorrent : Torrent -> TorrentStatus -> Html Msg
viewTorrent t s =
    Html.div [ HtmlAttr.class "torrentListEntry" ]
        [ Html.h3 [ HtmlAttr.class "torrentTitle" ] [ Html.text t.name ]
        , Html.progress [ HtmlAttr.value (toString s.downloaded), HtmlAttr.max "100"] []
        , Html.h5 [ HtmlAttr.class "torrentSubtitle"] [ Html.text ("Dimensiune: " ++ Filesize.format t.size) ]
        ]

viewTorrentDetails : Model -> Html Msg
viewTorrentDetails model =
    case model.torrentStatus of
        Nothing -> viewEmptyTorrentStatus
        Just x  -> 
            case model.torrent of
                Nothing -> viewEmptyTorrentStatus
                Just t  -> viewTorrentStatus t x 

viewTorrentStatus : Torrent -> TorrentStatus -> Html Msg
viewTorrentStatus t s =
    let v = Html.div [] 
                [ Html.h3 [] [ Html.text t.name ]
                , Html.hr [] []
                , statusField "Info hash" t.infoHash
                , statusField "Dimensiune" (Filesize.format t.size)
                , statusField "Dată adăugare" t.dateAdded
                , statusField "Progres" ((toString s.downloaded) ++ "%")
                , statusField "Viteză descărcare" (viewDownloadSpeed (toFloat s.downloadSpeed))
                ]
     in Html.div [ HtmlAttr.id "torrentStatus" ] [ v ]

viewDownloadSpeed : Float -> String
viewDownloadSpeed s =
    String.append (toString (s / 1000)) "Kb/s"

viewEmptyTorrentStatus = Html.div [ HtmlAttr.id "torrentStatus" ] []

statusField k v = 
    Html.div []
        [ Html.p [ HtmlAttr.class "statusValueKey" ] [ Html.text (String.append k ": ")]
        , Html.p [ HtmlAttr.class "statusValueField" ] [ Html.text v ]
        ]

        

button attrs = Html.div (List.append attrs [ HtmlAttr.class "btn" ])

