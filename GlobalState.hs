module GlobalState
where

import Data.IORef
import Graphics.UI.Gtk
import Graphics.UI.Gtk.Glade

import TV
import ListFilter

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