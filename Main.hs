module Main where

import Graphics.UI.Gtk hiding (fill)
import Graphics.UI.Gtk.Glade
import Graphics.UI.Gtk.ModelView as MV
import Control.Concurrent
import Control.Monad
import Data.Time
import Data.IORef
import Data.List

import TV
import DrawProgrammes
import Ranker
import ListFilter
-----------------------
chans = ["bbc1", "bbc2", "bbc3", "bbc4", "e4", "film_four", "virgin1", "more4", "dave"]
rankerFilename = "keywords.dat"
-----------------------

data GlobalState a = GlobalState {stXML :: GladeXML,
                                  stChans :: [Channel],
                                  stProgs :: [TVProgramme],
                                  stRanker :: a,
                                  stModels :: Maybe (TypedTreeModelSort TVProgramme, ListStore TVProgramme)
                                 }

type StateRef a = IORef (GlobalState a)

newGlobalStateRef :: Ranker a => 
                     GladeXML 
                  -> [Channel] 
                  -> [TVProgramme] 
                  -> a
                  -> IO (StateRef a)
newGlobalStateRef xml cs ps ra= newIORef (GlobalState {stXML = xml, 
                                                       stChans = cs, 
                                                       stProgs = ps,
                                                       stRanker = ra,
                                                       stModels = Nothing
                                                      })


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

  ranker <- (readIn rankerFilename) 
              `catch` (\_ -> return $ KeywordRanker([]))

  ref <- newGlobalStateRef xml cs ps ranker 

  setupProgramTreeView ref
  setupDrawingArea ref
  print "presetup"
  lstore' <- setupRecommendedProgrammes ref
  print "postsetup"
  lstore <- listFilterGetModel lstore'
  setupRankerOptions ref lstore

  widgetHide splash
  widgetShowAll window 


makeColumn :: (Ord o) => MV.TreeView -> MV.TypedTreeModelSort TVProgramme -> MV.ListStore TVProgramme -> String -> Int -> (TVProgramme -> String) -> (TVProgramme -> o) -> IO ()
makeColumn view model rawmodel title n text sortF = do
  treeSortableSetSortFunc model n $ \iter1 iter2 -> do
     p1 <- MV.treeModelGetRow rawmodel iter1
     p2 <- MV.treeModelGetRow rawmodel iter2
     return (compare (sortF p1) (sortF p2))
  col <- MV.treeViewColumnNew
  MV.treeViewColumnSetTitle col title
  rend <- MV.cellRendererTextNew
  MV.cellLayoutPackStart col rend True
  MV.cellLayoutSetAttributeFunc col rend model $ \iter -> do
     cIter <- MV.treeModelSortConvertIterToChildIter model iter
     p <- MV.treeModelGetRow rawmodel cIter
     set rend [MV.cellText := text p]
  MV.treeViewAppendColumn view col
  MV.treeViewColumnSetSortColumnId col n

insertModelsIntoRef ref model rawmodel = do
  gs <- readIORef ref
  newGS <- return $ gs {stModels = Just (model, rawmodel)}
  writeIORef ref newGS

setupProgramTreeView :: StateRef a -> IO ()
setupProgramTreeView ref = do
  xml <- liftM stXML $ readIORef ref
  ps <- liftM stProgs $ readIORef ref
  cs <- liftM stChans $ readIORef ref
  view <- xmlGetWidget xml MV.castToTreeView "programmeTreeView"
  
  rawmodel <- MV.listStoreNew ps
  model <- MV.treeModelSortNewWithModel rawmodel

  insertModelsIntoRef ref model rawmodel

  MV.treeSortableSetDefaultSortFunc model $ \iter1 iter2 -> do
     p1 <- MV.treeModelGetRow rawmodel iter1
     p2 <- MV.treeModelGetRow rawmodel iter2
     return (compare (start p1) (start p2))

  MV.treeViewSetModel view model

  makeColumn view model rawmodel "Title" 4 title title
  makeColumn view model rawmodel "Start" 5 (niceTime.start) start
  makeColumn view model rawmodel "Channel" 6 (channelName cs) (channelName cs)
  makeColumn view model rawmodel "Length" 7 ((++"m").show.programmeLengthMinutes) programmeLengthMinutes

  view `on` cursorChanged $ do
    showSelectedProgramme ref
  return ()

setupDrawingArea ref = do
  xml <- liftM stXML $ readIORef ref
  da <- xmlGetWidget xml castToDrawingArea "programInfo"
  da `onExpose` \_ -> showSelectedProgramme ref >> return True

setupRankerOptions ref lstore = do
  xml <- liftM stXML $ readIORef ref
  view <- xmlGetWidget xml castToTextView "keywordEntry"
  buffer <- textViewGetBuffer view

  KeywordRanker(kws) <- liftM stRanker $ readIORef ref
  textBufferSetText buffer $ show kws

  button <- xmlGetWidget xml castToButton "updateButton"
  onClicked button $ do
    text <- get buffer textBufferText
    gs <- readIORef ref
    kws' <- return $ (reads text :: [([(String, Float)], String)]) --WTF!?
    case kws' of  
      [] -> do
        kws <- liftM keywords (liftM stRanker $ readIORef ref)
        textBufferSetText buffer $ show kws
      _ -> do
        newgs <- writeIORef ref (gs{stRanker = (KeywordRanker(read text))})
        saveRankerOptions ref
        updateRecommendedProgrammes ref lstore
    return ()
  return ()
    
saveRankerOptions ref = do
    ranker <- liftM stRanker $ readIORef ref
    writeOut ranker rankerFilename

setupRecommendedProgrammes ref = do
  xml <- liftM stXML $ readIORef ref
  view <- xmlGetWidget xml castToTreeView "recommendedTreeView"
  ps <- liftM stProgs $ readIORef ref
  ranker <- liftM stRanker $ readIORef ref

  ranked <- return $ map (\p -> (p, (rank ranker p))) ps
  sorted <- return $ sortBy (\ (_,r) (_,r') -> compare r' r) ranked
  filtered <- return $ filter (\ (_,r) -> r > 0) sorted
              
  print "model"
  model <- MV.listStoreNew filtered
  print "filteredmodel"
  filteredmodel <- listFilterNewWithModel model [(\(p,_) -> title p == "Top Gear")]
  print "donefilteredmodel"
  print "stampresets are"
  printStamp filteredmodel
  MV.treeViewSetModel view filteredmodel
  print "done set"
  print "stampostsets are"
  printStamp filteredmodel



  namecol <- MV.treeViewColumnNew
  MV.treeViewColumnSetTitle namecol "Title"
  namerend <- MV.cellRendererTextNew
  MV.cellLayoutPackStart namecol namerend True
  MV.cellLayoutSetAttributeFunc namecol namerend filteredmodel $ \iter -> do
      (p,_) <- MV.treeModelGetRow filteredmodel iter
      set namerend [MV.cellText := title p]
  print "pre append col"
  MV.treeViewAppendColumn view namecol
  print "post apepnd col"

  rankcol <- MV.treeViewColumnNew
  MV.treeViewColumnSetTitle rankcol "Rank"
  rankrend <- MV.cellRendererTextNew
  MV.cellLayoutPackStart rankcol rankrend True
  MV.cellLayoutSetAttributeFunc rankcol rankrend filteredmodel $ \iter -> do
      (_,r) <- MV.treeModelGetRow filteredmodel iter
      set rankrend [MV.cellText := show r]
  MV.treeViewAppendColumn view rankcol
  
  view `on` cursorChanged $ do
    (path, col) <- MV.treeViewGetCursor view
    iter' <- MV.treeModelGetIter filteredmodel path
    case iter' of 
      Nothing -> return ()
      Just iter -> do
        (p,r) <- MV.treeModelGetRow filteredmodel iter
        selectProgramme ref p
  return filteredmodel

updateRecommendedProgrammes ref model = do
  xml <- liftM stXML $ readIORef ref
  view <- xmlGetWidget xml castToTreeView "recommendedTreeView"
  ps <- liftM stProgs $ readIORef ref
  ranker <- liftM stRanker $ readIORef ref 

  ranked <- return $ map (\p -> (p, (rank ranker p))) ps
  sorted <- return $ sortBy (\ (_,r) (_,r') -> compare r' r) ranked
  filtered <- return $ filter (\ (_,r) -> r > 0) sorted

  listStoreClear model
  mapM_ (listStoreAppend model) filtered
  
  return ()
  

showSelectedProgramme ref = do
  p' <- selectedProgramme ref
  case p' of Nothing -> return ()
             Just p -> displayProgrammeDetail ref p

selectedProgramme ref = do
  xml <- liftM stXML $ readIORef ref
  view <- xmlGetWidget xml MV.castToTreeView "programmeTreeView"
  models' <- liftM stModels $ readIORef ref
  case models' of 
    Nothing -> return Nothing
    Just (model,rawmodel) -> do
      (path, col) <- MV.treeViewGetCursor view
      iter' <- MV.treeModelGetIter model path
      case iter' of 
        Nothing -> return Nothing
        Just iter -> do
          cIter <- MV.treeModelSortConvertIterToChildIter model iter
          p <- MV.treeModelGetRow rawmodel cIter
          return (Just p)

selectProgramme ref p = do
  xml <- liftM stXML $ readIORef ref
  view <- xmlGetWidget xml MV.castToTreeView "programmeTreeView"
  models' <- liftM stModels $ readIORef ref
  case models' of 
    Nothing -> return ()
    Just (model,rawmodel) -> do
      fIter <- treeModelGetIterFirst model
      selectProgramme' view fIter model rawmodel p
  where
    selectProgramme' view iter' model rawmodel p = do
      case iter' of 
        Nothing -> return ()
        Just iter -> do
          cIter <- MV.treeModelSortConvertIterToChildIter model iter
          p' <- MV.treeModelGetRow rawmodel cIter
          if areTheSame p p'
             then do
               path <- MV.treeModelGetPath model iter
               MV.treeViewSetCursor view path Nothing
             else do
               nIter <- treeModelIterNext model iter
               selectProgramme' view nIter model rawmodel p
                   

--I WANT A TREE PATH!
-- find the correct iter for top level (model)

                  

displayProgrammeDetail ref p = do
  xml <- liftM stXML $ readIORef ref
  cs <- liftM stChans $ readIORef ref
  programInfo <- xmlGetWidget xml castToDrawingArea "programInfo"
  programmeInfoBubble programInfo cs p

filterOldProgrammes ps = do
  now <- liftM zonedTimeToLocalTime getZonedTime
  return [p | p<-ps, (stop p) > now]
