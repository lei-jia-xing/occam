-- Seed 01: High-Order CTEs, Recursive Unions & Window Analytics
-- Tests: Multiple CTE alias chaining, WITH RECURSIVE, window functions, and outer projection.

CREATE TABLE employees (
    emp_id INT PRIMARY KEY,
    manager_id INT REFERENCES employees(emp_id),
    emp_name VARCHAR(64) NOT NULL,
    department VARCHAR(32) NOT NULL,
    salary NUMERIC(10, 2) NOT NULL
);

INSERT INTO employees (emp_id, manager_id, emp_name, department, salary) VALUES
    (1, NULL, 'Alice', 'Engineering', 120000.00),
    (2, 1, 'Bob', 'Engineering', 95000.00),
    (3, 1, 'Charlie', 'Engineering', 90000.00),
    (4, 2, 'David', 'Engineering', 75000.00),
    (5, NULL, 'Eve', 'Marketing', 110000.00),
    (6, 5, 'Frank', 'Marketing', 80000.00);

WITH RECURSIVE org_hierarchy AS (
    SELECT emp_id, manager_id, emp_name, department, salary, 1 AS depth
    FROM employees
    WHERE manager_id IS NULL
    UNION ALL
    SELECT e.emp_id, e.manager_id, e.emp_name, e.department, e.salary, h.depth + 1
    FROM employees e
    JOIN org_hierarchy h ON e.manager_id = h.emp_id
),
dept_aggregates AS (
    SELECT
        department,
        count(*) AS headcount,
        avg(salary) AS avg_sal,
        max(salary) AS max_sal
    FROM org_hierarchy
    GROUP BY department
)
SELECT
    h.emp_name,
    h.department,
    h.salary,
    h.depth,
    d.avg_sal,
    row_number() OVER (PARTITION BY h.department ORDER BY h.salary DESC) AS dept_rank
FROM org_hierarchy h
JOIN dept_aggregates d ON h.department = d.department
WHERE h.salary > 80000.00;
