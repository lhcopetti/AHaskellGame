module Physics.DebugDraw.CircleDebugDraw
    ( mkCircleDebugDraw
    ) where

import SFML.Graphics.Color
import SFML.System.Vector2 (Vec2f (..))

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Maybe (MaybeT)

import GameObject.GameObjectTypes
import System.Messaging.Handler.RunMessageHandler (runMessageT)
import System.Messaging.Messages.ShapeMessage
import Component.Draw.CircleDrawing (createCenteredCircle)
import Component.Draw.LineDrawing (createLine)
import Component.Draw.CompositeDrawing (createComposite)
import Math.Cross (getUnitCrossScaled)


mkCircleDebugDraw :: Float -> MaybeT IO Drawing
mkCircleDebugDraw radius = do
    drw <- createCenteredCircle radius white
    liftIO $ do
        runMessageT (setFillColorMsg transparent) drw
        runMessageT (setOutlineThicknessMsg 1) drw
        runMessageT (setOutlineColorMsg green) drw
    cross <- mapM (createLineFlipped 1 red) (getUnitCrossScaled 3)
    createComposite $ drw : cross

createLineFlipped :: Float -> Color -> (Vec2f, Vec2f) -> MaybeT IO Drawing
createLineFlipped f c points = createLine points f c