CREATE TABLE users (id INT, name TEXT);
CREATE TABLE orders (id INT, user_id INT, amount INT);
CREATE TABLE products (id INT, title TEXT, price INT);
CREATE TABLE logs (id INT, msg TEXT);
SELECT * FROM products WHERE price > 100;
