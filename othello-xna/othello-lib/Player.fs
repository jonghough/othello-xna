module Player

type PlayerColor = | Black | White


type Player(playerColor : PlayerColor) =
    
    member this.playerColor = playerColor

    