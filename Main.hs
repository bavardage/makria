module Main where

import Graphics.UI.Gtk hiding (fill)
import Graphics.UI.Gtk.Glade
import Graphics.UI.Gtk.ModelView as MV
import Control.Concurrent
import Control.Monad
import Data.Time
import Data.IORef

import TV
import DrawProgrammes
-----------------------
chans = ["bbc1", "bbc3", "bbc2", "dave"]
-----------------------

data GlobalState = GlobalState {stXML :: GladeXML,
                                stChans :: [Channel],
                                stProgs :: [TVProgramme]
                               }

type StateRef = IORef GlobalState

newGlobalStateRef :: GladeXML -> [Channel] -> [TVProgramme] -> IO StateRef
newGlobalStateRef xml cs ps= newIORef (GlobalState {stXML = xml, stChans = cs, stProgs = ps})


main = doGUI 
  
doGUI = do
  initGUI

  Just xml <- xmlNew "TV-epg.glade"
  splash <- xmlGetWidget xml castToWindow "splash"

  widgetShowAll splash
  timeoutAddFull (yield >> return True)
                 priorityDefaultIdle 50
  forkIO (loadDataAndShowMainWindow splash xml chans)
  mainGUI

loadDataAndShowMainWindow splash xml channels = do
  window <- xmlGetWidget xml castToWindow "mainwindow"
  onDestroy window mainQuit

  (cs, ps') <- doXMLTV $ blebXMLTV channels

  ps <- filterOldProgrammes ps'

  ref <- newGlobalStateRef xml cs ps

  setupProgramTreeView ref

  widgetHide splash
  widgetShowAll window 


setupProgramTreeView :: StateRef -> IO ()
setupProgramTreeView ref = do
  xml <- liftM stXML $ readIORef ref
  ps <- liftM stProgs $ readIORef ref
  cs <- liftM stChans $ readIORef ref
  view <- xmlGetWidget xml MV.castToTreeView "programmeTreeView"
  
  rawmodel <- MV.listStoreNew ps
  model <- MV.treeModelSortNewWithModel rawmodel



  MV.treeSortableSetDefaultSortFunc model $ \iter1 iter2 -> do
     p1 <- MV.treeModelGetRow rawmodel iter1
     p2 <- MV.treeModelGetRow rawmodel iter2
     return (compare (start p1) (start p2))
  MV.treeSortableSetSortFunc model 1 $ \iter1 iter2 -> do
      p1 <- MV.treeModelGetRow rawmodel iter1
      p2 <- MV.treeModelGetRow rawmodel iter2
      return (compare (title p1) (title p2))
  MV.treeSortableSetSortFunc model 2 $ \iter1 iter2 -> do
      p1 <- MV.treeModelGetRow rawmodel iter1
      p2 <- MV.treeModelGetRow rawmodel iter2
      return (compare (start p1) (start p2))
  MV.treeSortableSetSortFunc model 3 $ \iter1 iter2 -> do
      p1 <- MV.treeModelGetRow rawmodel iter1
      p2 <- MV.treeModelGetRow rawmodel iter2
      return (compare (channelName cs p1) (channelName cs p2))

  MV.treeViewSetModel view model

  nameCol <- MV.treeViewColumnNew
  MV.treeViewColumnSetTitle nameCol "Title"

  timeCol <- MV.treeViewColumnNew
  MV.treeViewColumnSetTitle timeCol "Start"

  chanCol <- MV.treeViewColumnNew
  MV.treeViewColumnSetTitle chanCol "Channel"

  nameRenderer <- MV.cellRendererTextNew
  timeRenderer <- MV.cellRendererTextNew
  chanRenderer <- MV.cellRendererTextNew

  MV.cellLayoutPackStart nameCol nameRenderer True
  MV.cellLayoutPackStart timeCol timeRenderer True
  MV.cellLayoutPackStart chanCol chanRenderer True

  MV.cellLayoutSetAttributeFunc nameCol nameRenderer model $ \iter -> do
    cIter <- MV.treeModelSortConvertIterToChildIter model iter
    p <- MV.treeModelGetRow rawmodel cIter
    set nameRenderer [MV.cellText := title p]
  MV.cellLayoutSetAttributeFunc timeCol timeRenderer model $ \iter -> do
    cIter <- MV.treeModelSortConvertIterToChildIter model iter
    p <- MV.treeModelGetRow rawmodel cIter
    set timeRenderer [MV.cellText := show (start p)]
  MV.cellLayoutSetAttributeFunc chanCol chanRenderer model $ \iter -> do
    cIter <- MV.treeModelSortConvertIterToChildIter model iter
    p <- MV.treeModelGetRow rawmodel cIter
    set chanRenderer [MV.cellText := channelName cs p]


  MV.treeViewAppendColumn view nameCol
  MV.treeViewAppendColumn view timeCol
  MV.treeViewAppendColumn view chanCol

  MV.treeViewColumnSetSortColumnId nameCol 1
  MV.treeViewColumnSetSortColumnId timeCol 2
  MV.treeViewColumnSetSortColumnId chanCol 3

  
  return ()


filterOldProgrammes ps = do
  now <- liftM zonedTimeToLocalTime getZonedTime
  return [p | p<-ps, (stop p) > now]
