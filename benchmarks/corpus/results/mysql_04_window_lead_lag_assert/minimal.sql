-- Case ID: mysql_04
-- Target DBMS: MySQL (>= 8.0.18)
-- Bug Class: Assertion Failure (result_field != nullptr)
-- Defect Module: sql/item_window_func.cc
-- Reference: MySQL-BUG-97886
-- Description: Window function LEAD/LAG with default value expression having type
--              mismatch against partition column causing assertion failure during evaluation.

-- Auxiliary Setup Tables (Candidate for Dead-Table Slicing)




-- Target Reproduction Scenario: Stock tick pricing table with temporal window partitions
CREATE TABLE stock_ticks (
    tick_id INT PRIMARY KEY,
    symbol VARCHAR(8) NOT NULL,
    tick_time DATETIME NOT NULL,
    price_cents INT NOT NULL
);

INSERT INTO stock_ticks (tick_id, symbol, tick_time, price_cents) VALUES
    (1, 'AAPL', '2023-10-01 09:30:00', 17500),
    (2, 'AAPL', '2023-10-01 09:30:05', 17520),
    (3, 'AAPL', '2023-10-01 09:30:10', 17490),
    (4, 'MSFT', '2023-10-01 09:30:00', 31500),
    (5, 'MSFT', '2023-10-01 09:30:05', 31550);

-- Crashing Query: Window LEAD function specifying string default for numeric price column
SELECT
    tick_id,
    symbol,
    tick_time,
    price_cents,
    LEAD(price_cents, 1, 0) OVER (
        PARTITION BY symbol
       
    ) AS next_price,
    LAG(price_cents, 1, NULL) OVER (
        PARTITION BY symbol
       
    ) AS prev_price
FROM stock_ticks
;
