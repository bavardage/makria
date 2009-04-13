module ListFilter
(listFilterNewWithModel,
listFilterGetModel,
printStamp)
where


import Monad (liftM)
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

newtype ListFilter a = ListFilter (CustomStore (IORef (ListStore a, [(a->Bool)])) a)

instance TypedTreeModelClass ListFilter
instance TreeModelClass (ListFilter a)
instance GObjectClass (ListFilter a) where
  toGObject (ListFilter tm) = toGObject tm
  unsafeCastGObject = ListFilter . unsafeCastGObject

listFilterNewWithModel :: ListStore a -> [(a -> Bool)] -> IO (ListFilter a)
listFilterNewWithModel ls filters = do
  wrapped <- newIORef (ls, filters)
  flags <- liftM (\(x,_) -> treeModelGetFlags x) (readIORef wrapped)
  lf <- customStoreNew wrapped ListFilter TreeModelIface {
    treeModelIfaceGetFlags = return [TreeModelListOnly],

    treeModelIfaceGetIter = \[n] -> do
      (listStore,fs) <- readIORef wrapped
      frows <- filteredRows fs listStore                  
      if n < (length frows)
         then return $ Just (TreeIter 0 (fromIntegral n) 0 0)
         else return Nothing,

    treeModelIfaceGetPath = \(TreeIter s n _ _) -> do
      return [fromIntegral n],
      
    treeModelIfaceGetRow = \(TreeIter s n _ _) -> do
      (listStore,fs) <- readIORef wrapped
      frows <- filteredRows fs listStore 
      return (frows !! (fromIntegral n)),

    treeModelIfaceIterNext = \iter@(TreeIter s n _ _) -> do
      (listStore,fs) <- readIORef wrapped
      frows <- filteredRows fs listStore  
      if (fromIntegral (n+1)) < (length frows)
         then return $ Just (TreeIter s (n+1) 0 0)
         else return Nothing,

    treeModelIfaceIterChildren = \_ -> return Nothing,

    treeModelIfaceIterHasChild = \_ -> return False,

    treeModelIfaceIterNChildren = \n -> do
      (listStore,fs) <- readIORef wrapped
      frows <- filteredRows fs listStore                  
      return $ length frows,

    treeModelIfaceIterNthChild = \parent n -> do
      (listStore,fs) <- readIORef wrapped
      frows <- filteredRows fs listStore                  
      if n < (length frows)
         then return $ Just (TreeIter 0 (fromIntegral n) 0 0)
         else return Nothing,
                            
    treeModelIfaceIterParent = \_ -> return Nothing,

    treeModelIfaceRefNode = \_ -> return (),
    treeModelIfaceUnrefNode = \_ -> return ()
  } Nothing Nothing --no dragndrop for now

  ls `on` rowChanged $ \_ _ -> do
    print "ROW CHANGED OMG"
  ls `on` rowInserted $ \_ _ -> do
    print "ROW INSERTED"
  ls `on` rowHasChildToggled $ \_ _ -> do
    print "ROW HAS CHILD TOG"
  ls `on` rowDeleted $ \_ -> do
    print "ROW DELETED"
  ls `on` rowsReordered $ \_ _ _ ->  do
   print "REORDER"

  return lf

listFilterGetModel (ListFilter(customStore)) = do
  ref <- return $ customStoreGetPrivate customStore
  (listStore,_) <- readIORef ref
  return listStore

printStamp (ListFilter(customStore)) = do
  stamp <-customStoreGetStamp customStore
  print ("stamp is " ++ (show stamp))

filteredRows fs ls = do
  rows <- listStoreToList ls
  return $ filter (applyFilters fs) rows

applyFilters (f:fs) p = f p

