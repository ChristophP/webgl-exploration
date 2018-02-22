module Main exposing (main)

{-
   Rotating cube with colored sides.
-}

import AnimationFrame
import Color exposing (Color)
import Html exposing (Html, div, text)
import Html.Attributes exposing (height, style, width)
import Keyboard
import Math.Matrix4 as Mat4 exposing (Mat4)
import Math.Vector3 as Vec3 exposing (Vec3, vec3)
import OBJ
import OBJ.Types as OT
import Time exposing (Time)
import Util exposing ((=>))
import WebGL exposing (Mesh, Shader)


type alias Model =
    { theta : Float
    , fps : Float
    , keys : Keys
    , offset : Vec3
    , mesh : Mesh Vertex
    }


type alias Keys =
    { left : Bool
    , right : Bool
    , up : Bool
    , down : Bool
    }


type Msg
    = Animate Time
    | KeyChange Bool Keyboard.KeyCode
    | MeshLoaded (Result String (OT.MeshWith OT.Vertex))


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , subscriptions = subscriptions
        , update = update
        }


init : ( Model, Cmd Msg )
init =
    { theta = 0
    , fps = 0
    , keys = Keys False False False False
    , offset = vec3 0 -1 -4
    , mesh = WebGL.points []
    }
        => OBJ.loadMeshWithoutTexture "/src/Pot.obj" MeshLoaded


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ AnimationFrame.diffs Animate
        , Keyboard.downs (KeyChange True)
        , Keyboard.ups (KeyChange False)
        ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Animate dt ->
            { model
                | theta = model.theta + dt / 5000
                , fps = 1000 / dt
                , offset =
                    directionFromKeys model.keys
                        |> Vec3.scale (dt / 200)
                        |> Vec3.add model.offset
            }
                => Cmd.none

        KeyChange pressed code ->
            { model | keys = keyFunc pressed code model.keys } => Cmd.none

        MeshLoaded result ->
            case result of
                Ok mesh ->
                    let
                        verts =
                            List.map (\{ position } -> { position = position, color = vec3 0.5 0.5 0.5 }) mesh.vertices
                    in
                    { model | mesh = WebGL.indexedTriangles verts mesh.indices } => Cmd.none

                Err _ ->
                    model => Cmd.none


directionFromKeys : Keys -> Vec3
directionFromKeys { left, right, up, down } =
    let
        direction a b =
            if a == b then
                0
            else if a then
                -1
            else
                1
    in
    vec3 (direction left right) 0 (direction up down)


keyFunc : Bool -> Keyboard.KeyCode -> Keys -> Keys
keyFunc on keyCode keys =
    case keyCode of
        37 ->
            { keys | left = on }

        39 ->
            { keys | right = on }

        38 ->
            { keys | up = on }

        40 ->
            { keys | down = on }

        _ ->
            keys


view : Model -> Html Msg
view { theta, offset, mesh, fps } =
    div []
        [ div [] [ text <| "FPS: " ++ toString fps ]
        , WebGL.toHtml
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
                mesh
                (uniforms theta offset)
            ]
        ]


type alias Uniforms =
    { rotation : Mat4
    , perspective : Mat4
    , camera : Mat4
    , shade : Float
    , translation : Mat4
    }


uniforms : Float -> Vec3 -> Uniforms
uniforms theta offset =
    { rotation = Mat4.makeRotate (3 * theta) (vec3 0 1 0)

    --(Mat4.makeRotate (2 * theta) (vec3 1 0 0))
    , perspective = Mat4.makePerspective 45 1 0.01 100
    , camera = Mat4.makeLookAt (vec3 0 0 5) (vec3 0 0 0) (vec3 0 1 0)
    , shade = 0.8
    , translation = Mat4.makeTranslate offset
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
        uniform mat4 translation;
        varying vec3 vcolor;
        void main () {
            gl_Position = perspective * camera * translation * rotation * vec4(position, 1.0);
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
