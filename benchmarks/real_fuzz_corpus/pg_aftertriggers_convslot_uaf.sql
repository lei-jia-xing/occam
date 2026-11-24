CREATE TABLE parent (
col1 text primary key
);

CREATE TABLE child (
col1 text primary key,
FOREIGN KEY (col1) REFERENCES parent(col1) ON DELETE CASCADE
);

CREATE OR REPLACE FUNCTION trigger_function()
RETURNS TRIGGER
LANGUAGE PLPGSQL
AS $$
BEGIN
raise notice 'trigger = %, old table = %',
          TG_NAME,
          (select string_agg(old_table::text, ', ' order by col1) from old_table);
return NULL;
END;
$$;

CREATE TRIGGER bdt_trigger AFTER DELETE ON child REFERENCING OLD TABLE
AS old_table FOR EACH STATEMENT EXECUTE FUNCTION trigger_function();

ALTER TABLE child ADD COLUMN col2 text NOT NULL DEFAULT 'tutu';
INSERT INTO parent(col1) VALUES ('1');
INSERT INTO child(col1) VALUES ('1');
INSERT INTO parent(col1) VALUES ('2');
INSERT INTO child(col1) VALUES ('2');
DELETE FROM parent;
