package org.bosik.diacomp.core.persistence.serializers;

import java.util.List;
import org.bosik.diacomp.core.entities.business.foodbase.FoodItem;
import org.bosik.diacomp.core.entities.tech.Versioned;
import org.bosik.diacomp.core.persistence.parsers.Parser;
import org.bosik.diacomp.core.persistence.parsers.ParserFoodItem;
import org.bosik.diacomp.core.persistence.utils.ParserVersioned;
import org.bosik.diacomp.core.persistence.utils.SerializerAdapter;

public class SerializerFoodItem implements Serializer<Versioned<FoodItem>>
{
	private final Parser<FoodItem>					parser			= new ParserFoodItem();
	private final Parser<Versioned<FoodItem>>		parserVersioned	= new ParserVersioned<FoodItem>(parser);
	private final Serializer<Versioned<FoodItem>>	serializer		= new SerializerAdapter<Versioned<FoodItem>>(
																		parserVersioned);

	@Override
	public Versioned<FoodItem> read(String s)
	{
		return serializer.read(s);
	}

	@Override
	public List<Versioned<FoodItem>> readAll(String s)
	{
		return serializer.readAll(s);
	}

	@Override
	public String write(Versioned<FoodItem> object)
	{
		return serializer.write(object);
	}

	@Override
	public String writeAll(List<Versioned<FoodItem>> objects)
	{
		return serializer.writeAll(objects);
	}
}
