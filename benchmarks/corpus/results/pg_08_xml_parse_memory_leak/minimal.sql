-- Case ID: pg_08
-- Target DBMS: PostgreSQL (>= 11.0)
-- Bug Class: Memory Corruption / Double Free (xmlFreeDoc)
-- Defect Module: backend/utils/adt/xml.c
-- Reference: PG-BUG-16477
-- Description: xpath() evaluation on malformed XML entities leaking LibXML2 parser
--              contexts leading to memory corruption in allocator.

-- Auxiliary Setup Tables (Candidate for Dead-Table Slicing)
CREATE TABLE app_config_blobs (
    blob_id INT PRIMARY KEY,
    app_name VARCHAR(64),
    checksum CHAR(32)
);




-- Target Reproduction Scenario: XML documents and namespace-aware xpath evaluation


-- Crashing Query: Lateral unnesting of xpath with XML namespace array across malformed entities
