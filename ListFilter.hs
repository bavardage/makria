module ListFilter
    (ListFilter,
     listFilterNewWithModel,
     listFilterGetModel,
     listFilterAddFilter,
     listFilterRemoveAllFilters,
     listFilterSetFilters,
     printStamp)
where


import Monad (liftM,filterM)
import Data.IORef
import GHC.Word

import Graphics.UI.Gtk (on)
import Graphics.UI.Gtk.Types (GObjectClass(..), TreeModelClass)
import Graphics.UI.Gtk.ModelView.Types (TypedTreeModelClass, TreeIter(..))
import Graphics.UI.Gtk.ModelView.CustomStore
import Graphics.UI.Gtk.ModelView.ListStore
import Graphics.UI.Gtk.ModelView.TreeModel
import Graphics.UI.Gtk.ModelView.TreeDrag
import Control.Monad.Trans ( liftIO )

type Filter a = (a -> Bool)
newtype ListFilter a = ListFilter (CustomStore (IORef (ListStore a, [Filter a], [a])) a)

instance TypedTreeModelClass ListFilter
instance TreeModelClass (ListFilter a)
instance GObjectClass (ListFilter a) where
  toGObject (ListFilter tm) = toGObject tm
  unsafeCastGObject = ListFilter . unsafeCastGObject

listFilterNewWithModel :: ListStore a -> [Filter a] -> IO (ListFilter a)
listFilterNewWithModel ls filters = do
  frows <- filteredRows filters ls                  
  wrapped <- newIORef (ls, filters, frows)
  flags <- liftM (\(x,_,_) -> treeModelGetFlags x) (readIORef wrapped)
  lf <- customStoreNew wrapped ListFilter TreeModelIface {
    treeModelIfaceGetFlags = return [TreeModelListOnly],

    treeModelIfaceGetIter = \[n] -> do
--      print "getiter"
      (listStore,fs, frows) <- readIORef wrapped
      if n < (length frows)
         then return $ Just (TreeIter 0 (fromIntegral n) 0 0)
         else return Nothing,

    treeModelIfaceGetPath = \(TreeIter s n _ _) -> do
--      print "getpath"                                      
      return [fromIntegral n],
      
    treeModelIfaceGetRow = \(TreeIter s n _ _) -> do
      (listStore,fs,frows) <- readIORef wrapped
      return (frows !! (fromIntegral n)),

    treeModelIfaceIterNext = \iter@(TreeIter s n _ _) -> do
--      print "getiternext"
      (listStore,fs,frows) <- readIORef wrapped
      if (fromIntegral (n+1)) < (length frows)
         then return $ Just (TreeIter s (n+1) 0 0)
         else return Nothing,

    treeModelIfaceIterChildren = \_ -> return Nothing,

    treeModelIfaceIterHasChild = \_ -> return False,

    treeModelIfaceIterNChildren = \n -> do
  --    print "nchildren"
      (listStore,fs,frows) <- readIORef wrapped
      return $ length frows,

    treeModelIfaceIterNthChild = \parent n -> do
     -- print "nthchild"
      (listStore,fs,frows) <- readIORef wrapped
      if n < (length frows)
         then return $ Just (TreeIter 0 (fromIntegral n) 0 0)
         else return Nothing,
                            
    treeModelIfaceIterParent = \_ -> return Nothing,

    treeModelIfaceRefNode = \_ -> return (),
    treeModelIfaceUnrefNode = \_ -> return ()
  } Nothing Nothing --no dragndrop for now

  ls `on` rowInserted $ \_ _ -> do
    listFilterFiltersChanged lf --pretend the filters changed!
  ls `on` rowDeleted $ \_ -> do
    listFilterFiltersChanged lf
  return lf

  -- ls `on` rowHasChildToggled $ \_ _ -> do
  --   ----print "ROW HAS CHILD TOG"

  -- ls `on` rowsReordered $ \_ _ _ ->  do
  --  ----print "REORDER" -- DON'T CAER!!!
  -- ls `on` rowChanged $ \_ _ -> do
  --   ----print "ROW CHANGED OMG"


{-
listFilterUpdateFilteredNewFilter lf@(ListFilter(customStore)) =
    --reverse the thing you pass to it!!!!!!!!!
    let iteration self ref st@(ls,fs,rows) n = do
          x <- return $ rows !! n
          if applyFilters fs x
             then do --passes
               return st
             else do --noop
               rows' <- return $ (take n rows) ++ (drop (n+1) rows)
               st' <- return (ls,fs,rows')
               writeIORef ref st'
               treeModelRowDeleted self [n]
               return st'
        loop _    _   _ 0 = return ()
        loop self ref st n = do
          st' <- iteration self ref st n
          loop self ref st' (n-1)
                          
    in do
      ref <- return $ customStoreGetPrivate customStore
      st@(ls,fs,rows) <- readIORef ref
      if null rows
         then do
           return ()
         else loop lf ref st (length rows - 1)
-}
               

listFilterAddFilter lf@(ListFilter(customStore)) filter = do
  ref <- return $ customStoreGetPrivate customStore
  (listStore,filters,frows) <- readIORef ref
  writeIORef ref (listStore,filter:filters,frows)
  listFilterFiltersChanged lf

listFilterFiltersChanged lf@(ListFilter model) = 
    let removeAll lf@(ListFilter model) ls fs []  = return ()
        removeAll lf@(ListFilter model) ls fs rs  = do
          (ls,fs,rs) <- readIORef (customStoreGetPrivate model)
          rs' <- return $ init rs
          writeIORef (customStoreGetPrivate model) (ls,fs,rs')
          treeModelRowDeleted lf [(length rs - 1)]
          removeAll lf ls fs rs'
        addAll lf@(ListFilter model) [] n = return ()
        addAll lf@(ListFilter model) (x:xs) n = do
          (ls,fs,rs) <- readIORef (customStoreGetPrivate model)
          st' <- return $ (ls,fs,rs++[x])
          writeIORef (customStoreGetPrivate model) st'
          stamp <- (customStoreGetStamp model)
          treeModelRowInserted model [n] (TreeIter stamp (fromIntegral n) 0 0)
          addAll lf xs (n+1)
    in do
      st@(ls,fs,rs) <- readIORef (customStoreGetPrivate model)
      removeAll lf ls fs rs
      frows <- filteredRows fs ls
      addAll lf frows 0
      return ()

listFilterRemoveAllFilters lf@(ListFilter model) = do
  (ls,fs,rs) <- readIORef (customStoreGetPrivate model)
  writeIORef (customStoreGetPrivate model) (ls,[],rs)
  listFilterFiltersChanged lf

listFilterSetFilters lf@(ListFilter model) fs = do
  (ls,fs',rs) <- readIORef (customStoreGetPrivate model)
  writeIORef (customStoreGetPrivate model) (ls,fs,rs)
  listFilterFiltersChanged lf

listFilterGetModel (ListFilter(customStore)) = do
  ref <- return $ customStoreGetPrivate customStore
  (listStore,_,_) <- readIORef ref
  return listStore

printStamp (ListFilter(customStore)) = do
  stamp <-customStoreGetStamp customStore
  print ("stamp is " ++ (show stamp))

filteredRows fs ls = do
  rows <- listStoreToList ls
  return $ filter (applyFilters fs) rows

applyFilters [] _ = True
applyFilters (f:fs) p = f p

