CREATE TABLE tiny
(
    app_id String,
    date   Date,
    units  Nullable(UInt32),
    PROJECTION proj_by_app_id ( SELECT _part_offset ORDER BY app_id )
)
ENGINE = CoalescingMergeTree
ORDER BY app_id
SETTINGS deduplicate_merge_projection_mode = 'rebuild';

INSERT INTO tiny VALUES ('a','2024-01-01',1), ('b','2024-01-02',2);
INSERT INTO tiny VALUES ('a','2024-01-01',NULL), ('b','2024-01-02',20);

OPTIMIZE TABLE tiny FINAL;
