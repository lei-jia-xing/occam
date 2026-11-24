CREATE TABLE students (id INTEGER PRIMARY KEY, name VARCHAR);
CREATE TABLE subjects (id INTEGER PRIMARY KEY, name VARCHAR);
CREATE TABLE exams (
    exam_id INTEGER PRIMARY KEY,
    subject_id INTEGER REFERENCES subjects(id),
    student_id INTEGER REFERENCES students(id),
    grade INTEGER
);
INSERT INTO students VALUES (1, 'Student 1');
INSERT INTO subjects VALUES (1, 'CS 101');
INSERT INTO exams VALUES (1, 1, 1, 10);
SELECT
  (
    SELECT STRING_AGG(kcu2.column_name, ',' ORDER BY kcu2.ordinal_position)
    FROM information_schema.key_column_usage AS kcu2
    WHERE kcu2.constraint_name = rc.unique_constraint_name
  ) AS to_column,
  (
    SELECT kcu2.table_name
    FROM information_schema.key_column_usage AS kcu2
    WHERE kcu2.constraint_name = rc.unique_constraint_name LIMIT 1
  ) AS to_table
FROM
  information_schema.key_column_usage AS kcu
JOIN
  information_schema.table_constraints AS tc
ON
  tc.constraint_name = kcu.constraint_name
JOIN
  information_schema.referential_constraints AS rc
ON
  rc.constraint_name = kcu.constraint_name
WHERE
  tc.constraint_type = 'FOREIGN KEY' AND
  kcu.table_schema = 'main' AND
  kcu.table_name = 'exams' AND
  (to_column IS NOT NULL OR to_table IS NOT NULL)
GROUP BY
  kcu.constraint_schema,
  kcu.table_name,
  rc.unique_constraint_schema,
  rc.unique_constraint_name,
  tc.constraint_name,
  rc.update_rule,
  rc.delete_rule;
