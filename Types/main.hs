module Main2 where

type DiaLiteral = String
type DiaNumeral = Int 
type MesNumeral = Int
type AnioNumeral = Int
type DiasDelMes = Int

queDiaEs :: DiaNumeral -> DiaLiteral
queDiaEs x = case x of 
            1 -> "Domingo"
            2 -> "Lunes"
            3 -> "Martes"
            4 -> "Miercoles"
            5 -> "Jueves"
            6 -> "Viernes"
            7 -> "Sabado"
            _ -> "No se que dias es"

diasDelMes :: MesNumeral -> AnioNumeral -> DiasDelMes
diasDelMes m a = let 
                    esBiciesto = anioEsBiciesto a
                in
                    case m of 
                        1 -> 31
                        2 -> if esBiciesto then 29 else 28
                        3 -> 31
                        4 -> 30
                        5 -> 31
                        6 -> 30
                        7 -> 31
                        8 -> 31
                        9 -> 30
                        10 -> 31
                        11 -> 30
                        12 -> 31
                        _ -> -1
                where
                anioEsBiciesto :: AnioNumeral -> Bool
                anioEsBiciesto a = let
                                    entre4 = mod a 4 == 0
                                    entre100 = mod a 100 == 0 
                                    entre400 = mod a 400 == 0
                                    in
                                    if (entre4 && not entre100) || entre400
                                        then True
                                    else False
