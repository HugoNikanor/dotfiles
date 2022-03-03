{-
 - This is a reimplementation of XMonad.Prompt.MyWorkspace,
 - but with the addition of being able to set a prompt string
 -}
module XMonad.Prompt.MyWorkspace (workspacePrompt', workspacePrompt'', Wor'(Wor')) where

import XMonad hiding (workspaces)
import XMonad.Prompt
import XMonad.StackSet (workspaces, tag)
import XMonad.Util.WorkspaceCompare (getSortByIndex)

data Wor' = Wor' String
instance XPrompt Wor' where
    showXPrompt (Wor' x) = x

workspacePrompt' :: XPConfig -> (String -> X ()) -> X ()
workspacePrompt' conf job = do ws <- gets (workspaces . windowset)
                               sort <- getSortByIndex
                               let ts = map tag $ sort ws
                               mkXPrompt (Wor' "Test") conf (mkComplFunFromList' ts) job

workspacePrompt'' :: String -> XPConfig -> (String -> X ()) -> X ()
workspacePrompt'' prompt conf job =
    do ws <- gets (workspaces . windowset)
       sort <- getSortByIndex
       let ts = map tag $ sort ws
       mkXPrompt (Wor' prompt) conf (mkComplFunFromList' ts) job

