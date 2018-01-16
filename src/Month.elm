module Month exposing (Month, MonthId)


type alias Month =
    { month : MonthId
    , start : Int
    , end : Int
    }


type alias MonthId =
    String
