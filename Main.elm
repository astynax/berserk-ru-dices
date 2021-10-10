module Main exposing (main)

import Browser
import Collage as C exposing (defaultLineStyle)
import Collage.Events as Events
import Collage.Layout as Layout
import Collage.Render exposing (svgBox)
import Collage.Text as Text
import Color
import Glyph
import Html exposing (Html)
import Html.Attributes as HA
import Random


type alias Model =
    Maybe Throw


type Msg
    = Set Model
    | ThrowOne
    | ThrowTwo


type Strike
    = WeakStrike
    | MediumStrike
    | StrongStrike


type Throw
    = Strike Strike
    | CounterStrike
    | Miss


main : Program () Model Msg
main =
    Browser.document
        { init = \() -> ( Nothing, Cmd.none )
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }


update msg model =
    case msg of
        Set value ->
            ( value, Cmd.none )

        ThrowOne ->
            ( model, Random.generate (Set << Just) strikeThrow )

        ThrowTwo ->
            ( model, Random.generate (Set << Just) throw )


view : Model -> Browser.Document Msg
view model =
    { title = "Кубики для Берсерка"
    , body =
        [ Html.h3 []
            [ Html.text "Кубики для"
            , Html.br [] []
            , Html.text " Берсерка"
            ]
        , svgBox ( 220, 220 ) <|
            Layout.center <|
                Maybe.withDefault menu <|
                    Maybe.map (Events.onClick (Set Nothing) << result)
                        model
        , Html.a
            [ HA.href Glyph.sourceUrl
            , HA.style "font-size" "small"
            ]
            [ Html.text "Изображения взяты здесь" ]
        ]
    }


card w h =
    C.styled
        ( C.uniform <| Color.rgb255 255 240 200
        , { defaultLineStyle
            | thickness = C.thick
            , fill = C.uniform Color.black
          }
        )
    <|
        C.roundedRectangle w h 15


label =
    C.rendered
        << Text.size Text.large
        << Text.shape Text.Upright
        << Text.fromString


glyphBW =
    C.image ( 60, 60 ) Glyph.blackCounterStrike


glyphWW =
    C.image ( 60, 60 ) Glyph.whiteWeak


menu =
    Layout.vertical
        [ Events.onClick ThrowTwo <|
            menuItem <|
                Layout.horizontal
                    [ glyphBW, Layout.spacer 20 0, glyphWW ]
        , Events.onClick ThrowOne <|
            menuItem glyphWW
        ]


menuItem icons =
    Layout.impose (Layout.center icons) <| card 200 100


resultCard img text =
    Layout.impose
        (Layout.vertical
            [ C.image ( 60, 60 ) img
            , Layout.spacer 0 20
            , label text
            ]
        )
        (card 200 200)


result x =
    case x of
        Miss ->
            resultCard Glyph.blackMiss "Промах"

        CounterStrike ->
            resultCard Glyph.blackCounterStrike "Слабый ответ"

        Strike WeakStrike ->
            resultCard Glyph.whiteWeak "Слабый удар"

        Strike MediumStrike ->
            resultCard Glyph.whiteMedium "Средний удар"

        Strike StrongStrike ->
            resultCard Glyph.whiteStrong "Сильный удар"



-- generation


strikeThrow =
    Random.map Strike <|
        Random.uniform StrongStrike
            [ MediumStrike
            , MediumStrike
            , WeakStrike
            , WeakStrike
            , WeakStrike
            ]


throw =
    strikeThrow
        |> Random.andThen
            (\s -> Random.uniform CounterStrike [ Miss, s, s, s, s ])
