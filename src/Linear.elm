module Linear exposing
    ( Transform
    , Vector
    , add
    , compose
    , identity
    , rotation
    , scale
    , shift
    , transform
    )


type alias Vector =
    { x : Float, y : Float }


add : Vector -> Vector -> Vector
add v1 v2 =
    { x = v1.x + v2.x, y = v1.y + v2.y }


type Transform
    = Transform
        { c11 : Float
        , c12 : Float
        , c13 : Float
        , c21 : Float
        , c22 : Float
        , c23 : Float
        , c31 : Float
        , c32 : Float
        , c33 : Float
        }


scale : Float -> Float -> Transform
scale x y =
    Transform
        { c11 = x
        , c21 = 0
        , c31 = 0
        , c12 = 0
        , c22 = y
        , c32 = 0
        , c13 = 0
        , c23 = 0
        , c33 = 1
        }


shift : Float -> Float -> Transform
shift x y =
    Transform
        { c11 = 1
        , c21 = 0
        , c31 = 0
        , c12 = 0
        , c22 = 1
        , c32 = 0
        , c13 = x
        , c23 = y
        , c33 = 1
        }


rotation : Float -> Transform
rotation rad =
    Transform
        { c11 = cos rad
        , c21 = sin rad
        , c31 = 0
        , c12 = -(sin rad)
        , c22 = cos rad
        , c32 = 0
        , c13 = 0
        , c23 = 0
        , c33 = 1
        }


transform : Transform -> Vector -> Vector
transform (Transform t) v =
    { x = v.x * t.c11 + v.y * t.c12 + t.c13
    , y = v.x * t.c21 + v.y * t.c22 + t.c23
    }


compose : Transform -> Transform -> Transform
compose (Transform t2) (Transform t1) =
    Transform
        { c11 = t1.c11 * t2.c11 + t1.c12 * t2.c21 + t1.c13 * t2.c31
        , c12 = t1.c11 * t2.c12 + t1.c12 * t2.c22 + t1.c13 * t2.c32
        , c13 = t1.c11 * t2.c13 + t1.c12 * t2.c23 + t1.c13 * t2.c33
        , c21 = t1.c21 * t2.c11 + t1.c22 * t2.c21 + t1.c23 * t2.c31
        , c22 = t1.c21 * t2.c12 + t1.c22 * t2.c22 + t1.c23 * t2.c32
        , c23 = t1.c21 * t2.c13 + t1.c22 * t2.c23 + t1.c23 * t2.c33
        , c31 = t1.c31 * t2.c11 + t1.c32 * t2.c21 + t1.c33 * t2.c31
        , c32 = t1.c31 * t2.c12 + t1.c32 * t2.c22 + t1.c33 * t2.c32
        , c33 = t1.c31 * t2.c13 + t1.c32 * t2.c23 + t1.c33 * t2.c33
        }


identity : Transform
identity =
    Transform
        { c11 = 1
        , c12 = 0
        , c13 = 0
        , c21 = 0
        , c22 = 1
        , c23 = 0
        , c31 = 0
        , c32 = 0
        , c33 = 1
        }


zero : Vector
zero =
    { x = 0, y = 0 }


one : Vector
one =
    { x = 1, y = 1 }
