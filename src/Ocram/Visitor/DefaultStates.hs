module Ocram.Visitor.DefaultStates (
	EmptyDownState, emptyDownState
) where

import Ocram.Visitor.Visitor

newtype EmptyDownState = EmptyDownState ()

emptyDownState = EmptyDownState ()

instance DownVisitor EmptyDownState


