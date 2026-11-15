-- Seed 06: Views, Derived Tables & Complex Aggregations
-- Tests: CREATE VIEW dependencies, GROUP BY with HAVING,
-- multiple aggregate functions, and outer view queries.

CREATE TABLE order_lines (
    line_id INT PRIMARY KEY,
    order_id INT NOT NULL,
    product_cat VARCHAR(32) NOT NULL,
    unit_price NUMERIC(10, 2) NOT NULL,
    qty INT NOT NULL
);

INSERT INTO order_lines (line_id, order_id, product_cat, unit_price, qty) VALUES
    (1, 1001, 'Hardware', 25.00, 4),
    (2, 1001, 'Software', 150.00, 1),
    (3, 1002, 'Hardware', 40.00, 2),
    (4, 1003, 'Hardware', 10.00, 10),
    (5, 1003, 'Software', 300.00, 2);

CREATE VIEW v_category_summary AS
SELECT
    product_cat,
    count(DISTINCT order_id) AS distinct_orders,
    sum(unit_price * qty) AS total_revenue,
    avg(qty) AS avg_quantity
FROM order_lines
GROUP BY product_cat
HAVING sum(unit_price * qty) > 200.00;

SELECT product_cat, distinct_orders, total_revenue
FROM v_category_summary
WHERE total_revenue > 300.00;
