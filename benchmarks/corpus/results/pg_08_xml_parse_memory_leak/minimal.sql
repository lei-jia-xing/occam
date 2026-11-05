-- Case ID: pg_08
-- Target DBMS: PostgreSQL (>= 11.0)
-- Bug Class: Memory Corruption / Double Free (xmlFreeDoc)
-- Defect Module: backend/utils/adt/xml.c
-- Reference: PG-BUG-16477
-- Description: xpath() evaluation on malformed XML entities leaking LibXML2 parser
--              contexts leading to memory corruption in allocator.

-- Auxiliary Setup Tables (Candidate for Dead-Table Slicing)




-- Target Reproduction Scenario: XML documents and namespace-aware xpath evaluation
CREATE TABLE xml_payloads (
    payload_id INT PRIMARY KEY,
    service_origin VARCHAR(32),
    xml_content XML NOT NULL
);


-- Crashing Query: Lateral unnesting of xpath with XML namespace array across malformed entities
SELECT
    p.payload_id,
    p.service_origin,
    xpath_nodes.node_val
FROM xml_payloads p,
LATERAL unnest(
    xpath(
        '/soap:Envelope/soap:Body/item/@id',
        p.xml_content,
        ARRAY[ARRAY['soap', 'http://schemas.xmlsoap.org/soap/envelope/']]
    )
