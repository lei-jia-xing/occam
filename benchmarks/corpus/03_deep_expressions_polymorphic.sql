-- Seed 03: Deep AST Expression Trees, Multi-Branch CASE & Polymorphic Functions
-- Tests: Expression tree simplification, nested CASE WHEN, COALESCE, NULLIF,
-- and arithmetic/boolean operators reduction.

CREATE TABLE signal_metrics (
    metric_id INT PRIMARY KEY,
    raw_val NUMERIC(12, 4) NOT NULL,
    scale_factor INT NOT NULL,
    bias_offset NUMERIC(8, 2) NOT NULL,
    is_valid BOOLEAN NOT NULL,
    channel_mask INT NOT NULL
);

INSERT INTO signal_metrics (metric_id, raw_val, scale_factor, bias_offset, is_valid, channel_mask) VALUES
    (1, 105.4200, 10, 2.50, TRUE, 7),
    (2, 0.0000, 0, -1.00, FALSE, 0),
    (3, 850.1250, 100, 15.00, TRUE, 3);

SELECT
    metric_id,
    CASE
        WHEN is_valid AND scale_factor > 0 THEN
            COALESCE(
                NULLIF(raw_val * scale_factor + bias_offset, 0.0),
                CASE
                    WHEN (channel_mask & 1) = 1 THEN raw_val + 10.0
                    ELSE raw_val
                END
            )
        WHEN NOT is_valid THEN
            COALESCE(bias_offset, 0.0)
        ELSE
            0.0
    END AS calibrated_signal
FROM signal_metrics
WHERE (metric_id > 0 AND (channel_mask > 0 OR NOT is_valid))
ORDER BY metric_id;
