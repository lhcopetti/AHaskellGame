module Ball 
    ( Ball (..)
    , printSFML
    , createBall
    , drawBall
    , updateBall
    ) where
    
import SFML.Graphics.Color
import SFML.Graphics.CircleShape
import SFML.System.Vector2
import Vec2.Vec2Math (zero, addVec2f)
import SFML.Graphics.Types
import SFML.Graphics.RenderWindow (drawCircle)
import SFML.Graphics.SFShape (setFillColor)
import SFML.Graphics.SFTransformable (setPosition)

data Ball = Ball { circle   :: CircleShape
                 , position :: Vec2f
                 , velocity :: Vec2f
                 , color    :: Color
                 }


createBall :: Vec2f -> Vec2f -> IO (Maybe Ball)
createBall pos@(Vec2f x y) vel = do 
    putStrLn $ "Creating ball at " ++ show pos
    myCircle <- createCircleShape
    case myCircle of
        Left e -> do 
            putStrLn $ "Error while trying to create a circle shape. " ++ show e
            return Nothing
        Right r -> do
            let color = blue
            setFillColor r color
            setRadius r 25
            return $ Just (Ball r pos vel color)

drawBall :: RenderWindow -> Ball -> IO ()
drawBall wnd (Ball circle pos vel color) = drawCircle wnd circle Nothing


updateBall :: Vec2u -> Ball -> IO Ball
updateBall (Vec2u width height) (Ball c pos vel@(Vec2f velX velY) color) = do
    let newPos@(Vec2f x y) = addVec2f pos vel

    let newVelX = if x > fromIntegral width || x < 0 then (-velX) else velX
    let newVelY = if y > fromIntegral height || y < 0 then (-velY) else velY

    setPosition c newPos
    return (Ball c newPos (Vec2f newVelX newVelY) color)


printSFML :: IO ()
printSFML = putStrLn "Teste123"