module Main exposing (main)

{-
   Rotating cube with colored sides.
-}

import AnimationFrame
import Color exposing (Color)
import Html exposing (Html)
import Html.Attributes exposing (height, style, width)
import Math.Matrix4 as Mat4 exposing (Mat4)
import Math.Vector3 as Vec3 exposing (Vec3, vec3)
import Time exposing (Time)
import Util exposing ((=>))
import WebGL exposing (Mesh, Shader)


type alias Model =
    { theta : Float
    }


type Msg
    = FrameDiff Float


main : Program Never Model Msg
main =
    Html.program
        { init = ( Model 0, Cmd.none )
        , view = view
        , subscriptions = \_ -> AnimationFrame.diffs FrameDiff
        , update = update
        }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        FrameDiff dt ->
            ( { theta = model.theta + dt / 5000 }, Cmd.none )


view : Model -> Html Msg
view { theta } =
    WebGL.toHtml
        [ width 400
        , height 400
        , style
            [ "display" => "block"
            , "border" => "1px solid black"
            , "background-color" => "white"
            , "margin" => "auto"
            ]
        ]
        [ WebGL.entity
            vertexShader
            fragmentShader
            cubeMesh
            (uniforms theta)
        ]


type alias Uniforms =
    { rotation : Mat4
    , perspective : Mat4
    , camera : Mat4
    , shade : Float
    }


uniforms : Float -> Uniforms
uniforms theta =
    { rotation =
        Mat4.mul
            (Mat4.makeRotate (3 * theta) (vec3 0 1 0))
            (Mat4.makeRotate (2 * theta) (vec3 1 0 0))
    , perspective = Mat4.makePerspective 45 1 0.01 100
    , camera = Mat4.makeLookAt (vec3 0 0 5) (vec3 0 0 0) (vec3 0 1 0)
    , shade = 0.8
    }



-- Mesh


type alias Vertex =
    { color : Vec3
    , position : Vec3
    }


cubeMesh : Mesh Vertex
cubeMesh =
    let
        rft =
            vec3 1 1 1

        lft =
            vec3 -1 1 1

        lbt =
            vec3 -1 -1 1

        rbt =
            vec3 1 -1 1

        rbb =
            vec3 1 -1 -1

        rfb =
            vec3 1 1 -1

        lfb =
            vec3 -1 1 -1

        lbb =
            vec3 -1 -1 -1
    in
    [ face Color.green rft rfb rbb rbt
    , face Color.blue rft rfb lfb lft
    , face Color.yellow rft lft lbt rbt
    , face Color.red rfb lfb lbb rbb
    , face Color.purple lft lfb lbb lbt
    , face Color.orange rbt rbb lbb lbt
    ]
        |> List.concat
        |> WebGL.triangles


face : Color -> Vec3 -> Vec3 -> Vec3 -> Vec3 -> List ( Vertex, Vertex, Vertex )
face rawColor a b c d =
    let
        color =
            let
                c =
                    Color.toRgb rawColor
            in
            vec3
                (toFloat c.red / 255)
                (toFloat c.green / 255)
                (toFloat c.blue / 255)

        vertex position =
            Vertex color position
    in
    [ ( vertex a, vertex b, vertex c )
    , ( vertex c, vertex d, vertex a )
    ]



-- Shaders


vertexShader : Shader Vertex Uniforms { vcolor : Vec3 }
vertexShader =
    [glsl|

        attribute vec3 position;
        attribute vec3 color;
        uniform mat4 perspective;
        uniform mat4 camera;
        uniform mat4 rotation;
        varying vec3 vcolor;
        void main () {
            gl_Position = perspective * camera * rotation * vec4(position, 1.0);
            vcolor = color;
        }

    |]


fragmentShader : Shader {} Uniforms { vcolor : Vec3 }
fragmentShader =
    [glsl|

        precision mediump float;
        uniform float shade;
        varying vec3 vcolor;
        void main () {
            gl_FragColor = shade * vec4(vcolor, 1.0);
        }

    |]
