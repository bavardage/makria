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
--chans = ["bbc1", "bbc2", "bbc3", "bbc4", "e4", "film_four", "virgin1", "more4", "dave"]
chans = ["film_four", "bbc1"]
rankerFilename = "keywords.dat"
-----------------------

data GlobalState = GlobalState 
    {stXML :: GladeXML,
     stChans :: [Channel],
     stProgs :: [TVProgramme],
     stModels :: (TypedTreeModelSort TVProgramme, 
                  TypedTreeModelSort TVProgramme,
                  ListFilter TVProgramme,
                  ListStore TVProgramme)
                                 }

type StateRef a = IORef (GlobalState)

newGlobalStateRef xml cs ps mos = newIORef (GlobalState {stXML = xml, 
                                                       stChans = cs, 
                                                       stProgs = ps,
                                                       stModels = mos
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

  (cs, ps'') <- doXMLTV $ blebXMLTV channels
  ps' <- filterOldProgrammes ps''

  ranker <- (readIn rankerFilename) 
              `catch` (\ _ -> newKeywordRanker [])
  ps <- return $ map (\ p -> p{rankOf=(Just (rank ranker p))}) ps'

  models <- (createModelsFor ps) 
  ref <- newGlobalStateRef xml cs ps models

  setupProgramTreeView ref
  setupDrawingArea ref
  print "presetup"
  setupRecommendedProgrammes ref
  print "postsetup"
  setupRankerOptions ref ranker

  setupProgramTreeViewSignals ref
  setupRecommendedProgrammesSignals ref

  widgetHide splash
  widgetShowAll window 

createModelsFor ps = do
  rawmodel <- MV.listStoreNew ps
  filtermodel <- listFilterNewWithModel rawmodel []
  mainsortmodel <- MV.treeModelSortNewWithModel rawmodel
  secondsortmodel <- MV.treeModelSortNewWithModel filtermodel
  return  (mainsortmodel, secondsortmodel, filtermodel, rawmodel)


makeColumn :: (Ord o, 
               MV.TypedTreeModelClass ttm) => 
              MV.TreeView -> 
              MV.TypedTreeModelSort a -> 
              ttm a -> 
              String -> 
              Int -> 
             (a -> String) -> (a -> o) -> 
             IO (MV.CellRendererText,MV.TreeViewColumn)
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
  return (rend,col)

setupProgramTreeView :: StateRef a -> IO ()
setupProgramTreeView ref = do
  xml <- liftM stXML $ readIORef ref
  ps <- liftM stProgs $ readIORef ref
  cs <- liftM stChans $ readIORef ref
  (model,_,_,rawmodel) <- liftM stModels $ readIORef ref
  view <- xmlGetWidget xml MV.castToTreeView "programmeTreeView"
  
  MV.treeSortableSetDefaultSortFunc model $ \iter1 iter2 -> do
     p1 <- MV.treeModelGetRow rawmodel iter1
     p2 <- MV.treeModelGetRow rawmodel iter2
     return (compare (start p1) (start p2))

  MV.treeViewSetModel view model

  makeColumn view model rawmodel "Title" 4 title title
  makeColumn view model rawmodel "Start" 5 (niceTime.start) start
  makeColumn view model rawmodel "Channel" 6 (channelName cs) (channelName cs)
  makeColumn view model rawmodel "Length" 7 ((++"m").show.programmeLengthMinutes) programmeLengthMinutes
  return ()

setupProgramTreeViewSignals ref = do
  xml <- liftM stXML $ readIORef ref
  view <- xmlGetWidget xml MV.castToTreeView "programmeTreeView"
  view `on` cursorChanged $ do
    showSelectedProgramme ref
  return ()

setupDrawingArea ref = do
  xml <- liftM stXML $ readIORef ref
  da <- xmlGetWidget xml castToDrawingArea "programInfo"
  da `onExpose` \_ -> showSelectedProgramme ref >> return True

setupRecommendedProgrammes ref = do
  print "setting up recommended programmes"
  xml <- liftM stXML $ readIORef ref
  view <- xmlGetWidget xml castToTreeView "recommendedTreeView"
  (_,model,filtermodel,_) <- liftM stModels $ readIORef ref

  MV.treeSortableSetDefaultSortFunc model $ \iter1 iter2 -> do
    p1 <- MV.treeModelGetRow filtermodel iter1
    p2 <- MV.treeModelGetRow filtermodel iter2
    return (compare (rankOf p2) (rankOf p1))

  MV.treeViewSetModel view model

  listFilterAddFilter filtermodel ((>Just 0.5).rankOf)

  makeColumn view model filtermodel "Title" 1 title title
  makeColumn view model filtermodel "Time" 2 (niceTime.start) start
  makeColumn view model filtermodel "Rank" 3 showRank rankOf

  return () 
  where
    showRank p = showRank' $ (rankOf p)
    showRank' (Just r) = show r
    showRank' Nothing = "0"

setupRecommendedProgrammesSignals ref = do
  xml <- liftM stXML $ readIORef ref
  view <- xmlGetWidget xml castToTreeView "recommendedTreeView"
  (_,model,filtermodel,_) <- liftM stModels $ readIORef ref
  view `on` cursorChanged $ do
    (path, col) <- MV.treeViewGetCursor view
    iter' <- MV.treeModelGetIter model path
    case iter' of
      Nothing -> return ()
      Just iter -> do
        cIter <- MV.treeModelSortConvertIterToChildIter model iter
        p <- MV.treeModelGetRow filtermodel cIter
        selectProgramme ref p
      

setupRankerOptions ref ranker = do
  xml <- liftM stXML $ readIORef ref
  view <- xmlGetWidget xml MV.castToTreeView "keywordTreeView"
  newKeywordButton <- xmlGetWidget xml castToButton "newKeywordButton"
  updateButton <- xmlGetWidget xml castToButton "updateButton"
  deleteButton <- xmlGetWidget xml castToButton "deleteButton"
  keywords <- return $ keywords ranker

  rawmodel <- MV.listStoreNew keywords
  model <- MV.treeModelSortNewWithModel rawmodel

  MV.treeViewSetModel view model

  (keyrend, keycol) <- makeColumn view model rawmodel "Keyword" 1 fst fst
  set keyrend [cellTextEditable := True, cellTextEditableSet := True]
  (rankrend, rankcol) <- makeColumn view model rawmodel "Weighting" 2 (show.snd) snd
  set rankrend [cellTextEditable := True, cellTextEditableSet := True]
  
  on keyrend edited $ \path str -> do
    iter' <- MV.treeModelGetIter model path
    case iter' of
      Nothing -> return ()
      Just iter -> do
        cIter <- MV.treeModelSortConvertIterToChildIter model iter
        (_,r) <- MV.treeModelGetRow rawmodel cIter
        [n] <- MV.treeModelGetPath rawmodel cIter
        listStoreSetValue rawmodel n (str,r)
  on rankrend edited $ \path str -> do
    iter' <- MV.treeModelGetIter model path
    case iter' of
      Nothing -> return ()
      Just iter -> do
        cIter <- MV.treeModelSortConvertIterToChildIter model iter
        (kw,r) <- MV.treeModelGetRow rawmodel cIter
        [n] <- (MV.treeModelGetPath rawmodel cIter)
        r'' <- return (reads str :: [(Float,String)])
        case r'' of 
          [] -> listStoreSetValue rawmodel n (kw,r)
          ((r',_):_) -> listStoreSetValue rawmodel n (kw,r')

  (mainsortmodel,secondsortmodel,filtermodel,rawlistmodel) <- liftM stModels $ readIORef ref

  onClicked newKeywordButton $ do
    n <- listStoreAppend rawmodel ("",0)
    iter' <- treeModelIterNthChild rawmodel Nothing n
    case iter' of
      Nothing -> return ()
      Just iter -> do
        pIter <- treeModelSortConvertChildIterToIter model iter
        pPath <- treeModelGetPath model pIter
        treeViewSetCursor view pPath (Just (keycol,True))
    return ()

  onClicked updateButton $ do
    keywords <- listStoreToList rawmodel
    newkr <- newKeywordRanker keywords
    writeOut newkr rankerFilename

    ps' <- liftM stProgs $ readIORef ref
    ps <- return $ map (\p -> p{rankOf=(Just (rank newkr p))}) ps'
    gs <- readIORef ref
    newgs <- return $ gs {stProgs = ps}
    writeIORef ref newgs

    listStoreClear rawlistmodel
    mapM (listStoreAppend rawlistmodel) ps
    return ()
  
  onClicked deleteButton $ do
    (path,_) <- treeViewGetCursor view
    case path of
      [] -> return ()
      [n] -> do
        size <- listStoreGetSize rawmodel
        if n < size
           then listStoreRemove rawmodel n
           else return ()
      _ -> return ()

  return ()




showSelectedProgramme ref = do
  p' <- selectedProgramme ref
  case p' of Nothing -> return ()
             Just p -> displayProgrammeDetail ref p

selectedProgramme ref = do
  xml <- liftM stXML $ readIORef ref
  view <- xmlGetWidget xml MV.castToTreeView "programmeTreeView"
  (model,_,_,rawmodel) <- liftM stModels $ readIORef ref
  
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
  (model,_,_,rawmodel) <- liftM stModels $ readIORef ref
  
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