CREATE TABLE expr_calc_inputs (id INT PRIMARY KEY, base_val NUMERIC(12,4), factor_a INT, factor_b INT, flag_active BOOLEAN);

INSERT INTO expr_calc_inputs (id, base_val, factor_a, factor_b, flag_active) VALUES (1, 100.5000, 10, 5, TRUE), (2, 250.7500, 20, 0, FALSE);

SELECT
    id,
    CASE
        WHEN flag_active THEN
            COALESCE(
                NULLIF(base_val, 0.0),
                CASE
                    WHEN factor_a THEN base_val
                    ELSE base_val
                END
            )
        ELSE
            base_val
    END AS complex_calc
FROM expr_calc_inputs;
