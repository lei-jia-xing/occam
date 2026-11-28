-- | Semantic fact extraction for DDL statements (CREATE, DROP, ALTER, RENAME, ANALYZE).
module Occam.Frontend.Semantics.DDL (
    implicitCommit,
    indexSchemaPlacement,
    indexRef,
    triggerRef,
    qualifyRename,
    referentialAction,
    foreignKey,
    constraintFromTable,
    columnDefinition,
    tableDefinition,
    createTableFacts,
    createViewFacts,
    createIndexFacts,
    createTriggerFacts,
    createSequenceFacts,
    createSchemaFacts,
    createDatabaseFacts,
    createMacroFacts,
    createVirtualTableFacts,
    createRuleFacts,
    createStatisticsFacts,
    dropFacts,
    alterFacts,
    renameTableFacts,
    analyzeFacts,
) where

import Occam.Frontend.Semantics.DDL.AlterDrop
import Occam.Frontend.Semantics.DDL.Create
