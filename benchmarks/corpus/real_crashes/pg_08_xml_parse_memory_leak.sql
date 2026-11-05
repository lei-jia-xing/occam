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

CREATE TABLE event_logs (
    log_id BIGINT PRIMARY KEY,
    severity VARCHAR(16),
    message TEXT
);

INSERT INTO app_config_blobs (blob_id, app_name, checksum) VALUES
    (1, 'payment_svc', 'd41d8cd98f00b204e9800998ecf8427e'),
    (2, 'email_svc', '098f6bcd4621d373cade4e832627b4f6');

INSERT INTO event_logs (log_id, severity, message) VALUES
    (101, 'INFO', 'Worker initialized'),
    (102, 'WARN', 'Queue backlog high');

-- Target Reproduction Scenario: XML documents and namespace-aware xpath evaluation
CREATE TABLE xml_payloads (
    payload_id INT PRIMARY KEY,
    service_origin VARCHAR(32),
    xml_content XML NOT NULL
);

INSERT INTO xml_payloads (payload_id, service_origin, xml_content) VALUES
    (1, 'soap_gateway', '<soap:Envelope xmlns:soap="http://schemas.xmlsoap.org/soap/envelope/"><soap:Body><item id="101"><name>Gadget</name></item></soap:Body></soap:Envelope>'),
    (2, 'soap_gateway', '<soap:Envelope xmlns:soap="http://schemas.xmlsoap.org/soap/envelope/"><soap:Body><item id="102"><name>Widget</name></item><item id="103"/></soap:Body></soap:Envelope>'),
    (3, 'soap_gateway', '<root><unclosed_tag>data</root>');

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
) AS xpath_nodes(node_val)
WHERE xml_is_well_formed(p.xml_content::text);
