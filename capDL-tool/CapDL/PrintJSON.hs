{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}

{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module CapDL.PrintJSON
    ( printJSON
    ) where

import Control.Exception (assert)
import Data.Aeson (Key, ToJSON, Value(String), encode, toJSON, (.=))
import Data.ByteString.Lazy.Char8 (unpack)
import Data.Foldable
import Data.List
import Data.Maybe
import Data.Word (Word64)
import Data.Ord (comparing)
import Debug.Trace (traceShow)
import GHC.Generics (Generic)
import qualified Data.Aeson as Aeson
import qualified Data.Map as M
import qualified Data.Set as S

import CapDL.PrintUtils (sortObjects)
import qualified CapDL.Model as C

---

printJSON :: C.ObjectSizeMap -> C.Model Word -> String
printJSON = curry $ unpack . encode . uncurry render

---

type Badge = Word
type CPtr = Word

type ObjID = Integer
type CapSlot = Integer
type CapTable = [(CapSlot, Cap)]

data Spec = Spec
    { objects :: [NamedObject]
    , irqs :: [(Word, ObjID)]
    , asid_slots :: [ObjID]
    , root_objects :: Range ObjID
    , untyped_covers :: [UntypedCover]
    } deriving (Eq, Show, Generic, ToJSON)

data Range a = Range
    { start :: a
    , end :: a
    } deriving (Eq, Show, Generic, ToJSON)

data UntypedCover = UntypedCover
    { parent :: ObjID
    , children :: Range ObjID
    } deriving (Eq, Show, Generic, ToJSON)

data NamedObject = NamedObject
    { name :: String
    , object :: Object
    } deriving (Eq, Show, Generic, ToJSON)

data Object =
      Object_Untyped ObjectUntyped
    | Object_Endpoint
    | Object_Notification
    | Object_CNode ObjectCNode
    | Object_TCB ObjectTCB
    | Object_IRQ ObjectIRQ
    | Object_VCPU
    | Object_Frame ObjectFrame
    | Object_PageTable ObjectPageTable
    | Object_ASIDPool ObjectASIDPool
    | Object_ArmIRQ ObjectArmIRQ
    | Object_IRQMSI ObjectIRQMSI
    | Object_IRQIOAPIC ObjectIRQIOAPIC
    | Object_SchedContext ObjectSchedContext
    | Object_Reply
    deriving (Eq, Show)

instance ToJSON Object where
    toJSON obj = case obj of
        Object_Untyped obj -> tagged "Untyped" obj
        Object_Endpoint -> String "Endpoint"
        Object_Notification -> String "Notification"
        Object_CNode obj -> tagged "CNode" obj
        Object_TCB obj -> tagged "Tcb" obj
        Object_IRQ obj -> tagged "Irq" obj
        Object_VCPU -> String "VCpu"
        Object_Frame obj -> tagged "Frame" obj
        Object_PageTable obj -> tagged "PageTable" obj
        Object_ASIDPool obj -> tagged "AsidPool" obj
        Object_ArmIRQ obj -> tagged "ArmIrq" obj
        Object_IRQMSI obj -> tagged "IrqMsi" obj
        Object_IRQIOAPIC obj -> tagged "IrqIOApic" obj
        Object_SchedContext obj -> tagged "SchedContext" obj
        Object_Reply -> String "Reply"

data Cap =
      Cap_Untyped CapUntyped
    | Cap_Endpoint CapEndpoint
    | Cap_Notification CapNotification
    | Cap_CNode CapCNode
    | Cap_TCB CapTCB
    | Cap_IRQHandler CapIRQHandler
    | Cap_VCPU CapVCPU
    | Cap_Frame CapFrame
    | Cap_PageTable CapPageTable
    | Cap_ASIDPool CapASIDPool
    | Cap_ArmIRQHandler CapArmIRQHandler
    | Cap_IRQMSIHandler CapIRQMSIHandler
    | Cap_IRQIOAPICHandler CapIRQIOAPICHandler
    | Cap_SchedContext CapSchedContext
    | Cap_Reply CapReply
    deriving (Eq, Show)

instance ToJSON Cap where
    toJSON cap' = case cap' of
        Cap_Untyped cap -> tagged "Untyped" cap
        Cap_Endpoint cap -> tagged "Endpoint" cap
        Cap_Notification cap -> tagged "Notification" cap
        Cap_CNode cap -> tagged "CNode" cap
        Cap_TCB cap -> tagged "Tcb" cap
        Cap_IRQHandler cap -> tagged "IrqHandler" cap
        Cap_VCPU cap -> tagged "VCpu" cap
        Cap_Frame cap -> tagged "Frame" cap
        Cap_PageTable cap -> tagged "PageTable" cap
        Cap_ASIDPool cap -> tagged "AsidPool" cap
        Cap_ArmIRQHandler cap -> tagged "ArmIrqHandler" cap
        Cap_IRQMSIHandler cap -> tagged "IrqMsiHandler" cap
        Cap_IRQIOAPICHandler cap -> tagged "IrqIOApicHandler" cap
        Cap_SchedContext cap -> tagged "SchedContext" cap
        Cap_Reply cap -> tagged "Reply" cap

data Rights = Rights
    { rights_read :: Bool
    , rights_write :: Bool
    , rights_grant :: Bool
    , rights_grant_reply :: Bool
    } deriving (Eq, Show)

-- HACK until NoFieldSelectors is available
instance ToJSON Rights where
    toJSON Rights {..} = Aeson.object
        [ "read" .= rights_read
        , "write" .= rights_write
        , "grant" .= rights_grant
        , "grant_reply" .= rights_grant_reply
        ]

emptyRights :: Rights
emptyRights = Rights False False False False

data FrameInit = FrameInit Fill
    deriving (Eq, Show)

instance ToJSON FrameInit where
    toJSON (FrameInit fill) = tagged "Fill" fill

data Fill = Fill
    { entries :: [FillEntry]
    } deriving (Eq, Show, Generic, ToJSON)

data FillEntry = FillEntry
    { range :: FillEntryRange
    , content :: FillEntryContent
    } deriving (Eq, Show, Generic, ToJSON)

data FillEntryRange = FillEntryRange
    { start :: Word
    , end :: Word
    } deriving (Eq, Show, Generic, ToJSON)

data FillEntryContent =
      FillEntryContent_Data FillEntryContentFile
    | FillEntryContent_BootInfo FillEntryContentBootInfo
    deriving (Eq, Show)

instance ToJSON FillEntryContent where
    toJSON content = case content of
        FillEntryContent_Data file -> tagged "Data" file
        FillEntryContent_BootInfo bootinfo -> tagged "BootInfo" bootinfo

data FillEntryContentFile = FillEntryContentFile
    { file :: String
    , file_offset :: Word
    } deriving (Eq, Show, Generic, ToJSON)

data FillEntryContentBootInfo = FillEntryContentBootInfo
    { id :: FillEntryContentBootInfoId
    , offset :: Word
    } deriving (Eq, Show, Generic, ToJSON)

data FillEntryContentBootInfoId =
      Padding
    | Fdt
    deriving (Eq, Show, Generic, ToJSON)

data ObjectUntyped = ObjectUntyped
    { size_bits :: Word
    , paddr :: Maybe Word
    } deriving (Eq, Show, Generic, ToJSON)

data ObjectCNode = ObjectCNode
    { size_bits :: Word
    , slots :: CapTable
    } deriving (Eq, Show, Generic, ToJSON)

data ObjectTCB = ObjectTCB
    { slots :: CapTable
    , extra :: ObjectTCBExtraInfo
    } deriving (Eq, Show, Generic, ToJSON)

data ObjectTCBExtraInfo = ObjectTCBExtraInfo
    { ipc_buffer_addr :: Word
    , affinity :: Word
    , prio :: Word
    , max_prio :: Word
    , resume :: Bool
    , ip :: Word
    , sp :: Word
    , gprs :: [Word]
    , master_fault_ep :: CPtr
    } deriving (Eq, Show, Generic, ToJSON)

data ObjectIRQ = ObjectIRQ
    { slots :: CapTable
    } deriving (Eq, Show, Generic, ToJSON)

data ObjectFrame = ObjectFrame
    { size_bits :: Word
    , paddr :: Maybe Word
    , init :: FrameInit
    } deriving (Eq, Show, Generic, ToJSON)

data ObjectPageTable = ObjectPageTable
    { is_root :: Bool
    , level :: Maybe Int
    , slots :: CapTable
    } deriving (Eq, Show, Generic, ToJSON)

data ObjectASIDPool = ObjectASIDPool
    { high :: Word
    } deriving (Eq, Show, Generic, ToJSON)

data ObjectArmIRQ = ObjectArmIRQ
    { slots :: CapTable
    , extra :: ObjectArmIRQExtraInfo
    } deriving (Eq, Show, Generic, ToJSON)

data ObjectArmIRQExtraInfo = ObjectArmIRQExtraInfo
    { trigger :: Word
    , target :: Word
    } deriving (Eq, Show, Generic, ToJSON)

data ObjectIRQMSI = ObjectIRQMSI
    { slots :: CapTable
    , extra :: ObjectIRQMSIExtraInfo
    } deriving (Eq, Show, Generic, ToJSON)

data ObjectIRQMSIExtraInfo = ObjectIRQMSIExtraInfo
    { handle:: Word
    , pci_bus :: Word
    , pci_dev :: Word
    , pci_func :: Word
    } deriving (Eq, Show, Generic, ToJSON)

data ObjectIRQIOAPIC = ObjectIRQIOAPIC
    { slots :: CapTable
    , extra :: ObjectIRQIOAPICExtraInfo
    } deriving (Eq, Show, Generic, ToJSON)

data ObjectIRQIOAPICExtraInfo = ObjectIRQIOAPICExtraInfo
    { ioapic :: Word
    , pin :: Word
    , level :: Word
    , polarity :: Word
    } deriving (Eq, Show, Generic, ToJSON)

data ObjectSchedContext = ObjectSchedContext
    { size_bits :: Word
    , extra :: ObjectSchedContextExtraInfo
    } deriving (Eq, Show, Generic, ToJSON)

data ObjectSchedContextExtraInfo = ObjectSchedContextExtraInfo
    { period :: Word64
    , budget :: Word64
    , badge :: Badge
    } deriving (Eq, Show, Generic, ToJSON)

data CapUntyped = CapUntyped
    { object :: ObjID
    } deriving (Eq, Show, Generic, ToJSON)

data CapEndpoint = CapEndpoint
    { object :: ObjID
    , badge :: Badge
    , rights :: Rights
    } deriving (Eq, Show, Generic, ToJSON)

data CapNotification = CapNotification
    { object :: ObjID
    , badge :: Badge
    , rights :: Rights
    } deriving (Eq, Show, Generic, ToJSON)

data CapCNode = CapCNode
    { object :: ObjID
    , guard :: Word
    , guard_size :: Word
    } deriving (Eq, Show, Generic, ToJSON)

data CapTCB = CapTCB
    { object :: ObjID
    } deriving (Eq, Show, Generic, ToJSON)

data CapIRQHandler = CapIRQHandler
    { object :: ObjID
    } deriving (Eq, Show, Generic, ToJSON)

data CapVCPU = CapVCPU
    { object :: ObjID
    } deriving (Eq, Show, Generic, ToJSON)

data CapFrame = CapFrame
    { object :: ObjID
    , rights :: Rights
    , cached :: Bool
    } deriving (Eq, Show, Generic, ToJSON)

data CapPageTable = CapPageTable
    { object :: ObjID
    } deriving (Eq, Show, Generic, ToJSON)

data CapASIDPool = CapASIDPool
    { object :: ObjID
    } deriving (Eq, Show, Generic, ToJSON)

data CapArmIRQHandler = CapArmIRQHandler
    { object :: ObjID
    } deriving (Eq, Show, Generic, ToJSON)

data CapIRQMSIHandler = CapIRQMSIHandler
    { object :: ObjID
    } deriving (Eq, Show, Generic, ToJSON)

data CapIRQIOAPICHandler = CapIRQIOAPICHandler
    { object :: ObjID
    } deriving (Eq, Show, Generic, ToJSON)

data CapSchedContext = CapSchedContext
    { object :: ObjID
    } deriving (Eq, Show, Generic, ToJSON)

data CapReply = CapReply
    { object :: ObjID
    } deriving (Eq, Show, Generic, ToJSON)

tagged :: ToJSON a => Key -> a -> Value
tagged tag value = Aeson.object [ tag .= toJSON value ]

---

render :: C.ObjectSizeMap -> C.Model Word -> Spec
render objSizeMap (C.Model arch objMap irqNode _ coverMap) = Spec
    { objects
    , irqs
    , asid_slots = asidSlots
    , root_objects = Range 0 (toInteger numRootObjects)
    , untyped_covers = untypedCovers
    }
  where
    orderedObjectIds = rootObjectIds ++ childObjectIds

    numRootObjects = length rootObjectIds

    rootObjectIds = map fst sorted
      where
        allChildren = S.fromList . concat $ M.elems coverMap
        unsorted = filter (flip S.notMember allChildren) (M.keys objMap)
        sorted = sortObjects objSizeMap [ (objId, objMap M.! objId) | objId <- unsorted ]

    (_, childObjectIds, untypedCovers) = foldr f (numRootObjects, [], []) (concatMap M.toList (objectLayers coverMap))
      where
        f (parent, children) (n, allChildren, covers) =
            ( n + length children
            , allChildren ++ children
            , covers ++ [UntypedCover (renderId parent) (Range (toInteger n) (toInteger (n + length children)))]
            )

    renderId = (M.!) (M.fromList (zip orderedObjectIds [0..]))
    renderCapTable = M.toList . M.map renderCap . M.mapKeys toInteger

    pageTableIsVSpace = flip S.member . S.fromList $ do
        C.TCB { slots } <- M.elems objMap
        C.PTCap { capObj } <- return $ slots M.! C.tcbVTableSlot
        return capObj

    irqs =
        [ (irq, renderId obj)
        | (irq, obj) <- M.toAscList irqNode
        ]

    asidSlots = assert (map fst table `isPrefixOf` [1..]) (map snd table)
      where
        table = sortBy (comparing fst)
            [ let Just asidHigh = maybeAsidHigh
              in assert (M.null lowSlots) (asidHigh, renderId objID)
            | (objID, C.ASIDPool lowSlots maybeAsidHigh) <- M.toList objMap
            ]

    objects =
        [ NamedObject
            { name = renderName objId
            , object = renderObj objId
            }
        | objId <- orderedObjectIds
        ]

    renderObj objId = case objMap M.! objId of
        C.Untyped { maybeSizeBits = Just sizeBits, maybePaddr } -> Object_Untyped (ObjectUntyped sizeBits maybePaddr)
        C.Endpoint -> Object_Endpoint
        C.Notification -> Object_Notification
        C.Frame { vmSizeBits, maybePaddr, maybeFill } -> Object_Frame (ObjectFrame vmSizeBits maybePaddr (renderFrameInit maybeFill))
        C.PT slots -> Object_PageTable $
            let renderedSlots = renderCapTable slots
            in case arch of
                C.RISCV -> ObjectPageTable { is_root = pageTableIsVSpace objId, level = Nothing, slots = renderedSlots }
                _ -> ObjectPageTable { is_root = False, level = Just 3, slots = renderedSlots }
        C.PD slots -> Object_PageTable $
            let renderedSlots = renderCapTable slots
            in case arch of
                C.ARM11 -> ObjectPageTable { is_root = True, level = Just 0, slots = renderedSlots }
                _ -> ObjectPageTable { is_root = False, level = Just 2, slots = renderedSlots }
        C.PUD slots -> Object_PageTable (ObjectPageTable { is_root = False, level = Just 1, slots = renderCapTable slots })
        C.PGD slots -> Object_PageTable (ObjectPageTable { is_root = True, level = Just 0, slots = renderCapTable slots })
        C.PDPT slots -> Object_PageTable (ObjectPageTable { is_root = False, level = Just 1, slots = renderCapTable slots })
        C.PML4 slots -> Object_PageTable (ObjectPageTable { is_root = True, level = Just 0, slots = renderCapTable slots })
        C.CNode slots 0 -> Object_IRQ (ObjectIRQ (renderCapTable slots)) -- model uses 0-sized CNodes as token objects for IRQs
        C.CNode slots sizeBits -> Object_CNode (ObjectCNode sizeBits (renderCapTable slots))
        C.VCPU -> Object_VCPU
        C.ARMIrq slots trigger target -> Object_ArmIRQ (ObjectArmIRQ (renderCapTable slots) (ObjectArmIRQExtraInfo trigger target))
        C.MSIIrq slots handle bus dev fun -> Object_IRQMSI (ObjectIRQMSI (renderCapTable slots) (ObjectIRQMSIExtraInfo handle bus dev fun))
        C.IOAPICIrq slots ioapic pin ioapic_level polarity -> Object_IRQIOAPIC (ObjectIRQIOAPIC (renderCapTable slots) (ObjectIRQIOAPICExtraInfo ioapic pin ioapic_level polarity))
        C.ASIDPool slots (Just asidHigh) -> assert (M.null slots) Object_ASIDPool (ObjectASIDPool asidHigh)
        C.RTReply -> Object_Reply
        C.TCB
            { slots
            , faultEndpoint
            , extraInfo = Just extraInfo
            , initArguments
            } ->
            let C.TCBExtraInfo
                    { ipcBufferAddr
                    , ip = Just ip
                    , sp = Just sp
                    , prio = Just prio
                    , max_prio = Just max_prio
                    , affin = Just affinity
                    , resume
                    } = extraInfo
            in Object_TCB (ObjectTCB
                { slots = renderCapTable slots
                , extra = ObjectTCBExtraInfo
                    { ipc_buffer_addr = ipcBufferAddr
                    , affinity = fromIntegral affinity
                    , prio = fromIntegral prio
                    , max_prio = fromIntegral max_prio
                    , resume = fromMaybe True resume
                    , ip
                    , sp
                    , gprs = initArguments
                    , master_fault_ep = fromMaybe 0 faultEndpoint
                    }
                })
        C.SC
            { maybeSizeBits = Just sizeBits
            , sc_extraInfo = Just extraInfo
            } ->
            let C.SCExtraInfo
                    { period = Just period
                    , budget = Just budget
                    , scData = Just badge
                    } = extraInfo
            in Object_SchedContext (ObjectSchedContext
                { size_bits = sizeBits
                , extra = ObjectSchedContextExtraInfo
                    { period
                    , budget
                    , badge
                    }
                })
        x -> traceShow x undefined

    renderCap cap = case cap of
        C.UntypedCap capObj -> Cap_Untyped (CapUntyped (renderId capObj))
        C.EndpointCap capObj capBadge capRights -> Cap_Endpoint (CapEndpoint (renderId capObj) capBadge (renderRights capRights))
        C.NotificationCap capObj capBadge capRights -> Cap_Notification (CapNotification (renderId capObj) capBadge (renderRights capRights))
        C.CNodeCap capObj capGuard capGuardSize -> Cap_CNode (CapCNode (renderId capObj) capGuard capGuardSize)
        C.TCBCap capObj -> Cap_TCB (CapTCB (renderId capObj))
        C.IRQHandlerCap capObj -> Cap_IRQHandler (CapIRQHandler (renderId capObj))
        C.VCPUCap capObj -> Cap_VCPU (CapVCPU (renderId capObj))
        C.FrameCap { capObj, capRights, capCached } -> Cap_Frame (CapFrame (renderId capObj) (renderRights capRights) capCached)
        C.PTCap capObj _ -> Cap_PageTable (CapPageTable (renderId capObj))
        C.PDCap capObj _ -> Cap_PageTable (CapPageTable (renderId capObj))
        C.PUDCap capObj _ -> Cap_PageTable (CapPageTable (renderId capObj))
        C.PGDCap capObj _ -> Cap_PageTable (CapPageTable (renderId capObj))
        C.PDPTCap capObj _ -> Cap_PageTable (CapPageTable (renderId capObj))
        C.PML4Cap capObj _ -> Cap_PageTable (CapPageTable (renderId capObj))
        C.ARMIRQHandlerCap capObj -> Cap_ArmIRQHandler (CapArmIRQHandler (renderId capObj))
        C.IRQMSIHandlerCap capObj -> Cap_IRQMSIHandler (CapIRQMSIHandler (renderId capObj))
        C.IRQIOAPICHandlerCap capObj -> Cap_IRQIOAPICHandler (CapIRQIOAPICHandler (renderId capObj))
        C.ASIDPoolCap capObj -> Cap_ASIDPool (CapASIDPool (renderId capObj))
        C.SCCap capObj -> Cap_SchedContext (CapSchedContext (renderId capObj))
        C.RTReplyCap capObj -> Cap_Reply (CapReply (renderId capObj))
        x -> traceShow x undefined

renderName :: C.ObjID -> String
renderName (name, Nothing) = name

renderFrameInit :: Maybe [[String]] -> FrameInit
renderFrameInit = FrameInit . Fill . map f . concat . toList
  where
    f (dest_offset:dest_len:rest) = FillEntry
        { range = FillEntryRange { start = start, end = end }
        , content
        }
      where
        start = read dest_offset
        len = read dest_len
        end = start + len
        content = case rest of
            "CDL_FrameFill_FileData":file:file_offset:[] -> FillEntryContent_Data
                (FillEntryContentFile
                    { file = tail (Data.List.init file)
                    , file_offset = read file_offset
                    })
            "CDL_FrameFill_BootInfo":id:offset:[] -> FillEntryContent_BootInfo
                (FillEntryContentBootInfo
                    { id = case id of
                        "CDL_FrameFill_BootInfo_FDT" -> Fdt
                    , offset = read offset
                    })

renderRights :: C.CapRights -> Rights
renderRights = foldr f emptyRights
    where
    f right acc = case right of
        C.Read -> acc { rights_read = True }
        C.Write -> acc { rights_write = True }
        C.Grant -> acc { rights_grant = True }
        C.GrantReply -> acc { rights_grant_reply = True }

objectLayers :: C.CoverMap -> [C.CoverMap]
objectLayers = unfoldr step
  where
    step :: C.CoverMap -> Maybe (C.CoverMap, C.CoverMap)
    step intermediate =
        if M.null intermediate
        then Nothing
        else
            let children = S.fromList . concat $ M.elems intermediate
            in  Just $ M.partitionWithKey (const . not . (`S.member` children)) intermediate
