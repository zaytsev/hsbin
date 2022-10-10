CREATE TABLE paste_item (
	id int NOT NULL GENERATED ALWAYS AS IDENTITY (START 10000),
	title text,
	body text,
	
	PRIMARY KEY(id)
);
