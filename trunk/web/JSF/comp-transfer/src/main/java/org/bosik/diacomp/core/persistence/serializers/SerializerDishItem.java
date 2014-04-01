package org.bosik.diacomp.core.persistence.serializers;

import java.util.List;
import org.bosik.diacomp.core.entities.business.dishbase.DishItem;
import org.bosik.diacomp.core.entities.tech.Versioned;
import org.bosik.diacomp.core.persistence.parsers.Parser;
import org.bosik.diacomp.core.persistence.parsers.ParserDishItem;
import org.bosik.diacomp.core.persistence.utils.ParserVersioned;
import org.bosik.diacomp.core.persistence.utils.SerializerAdapter;

public class SerializerDishItem implements Serializer<Versioned<DishItem>>
{
	private final Parser<DishItem>					parser			= new ParserDishItem();
	private final Parser<Versioned<DishItem>>		parserVersioned	= new ParserVersioned<DishItem>(parser);
	private final Serializer<Versioned<DishItem>>	serializer		= new SerializerAdapter<Versioned<DishItem>>(
																		parserVersioned);

	@Override
	public Versioned<DishItem> read(String s)
	{
		return serializer.read(s);
	}

	@Override
	public List<Versioned<DishItem>> readAll(String s)
	{
		return serializer.readAll(s);
	}

	@Override
	public String write(Versioned<DishItem> object)
	{
		return serializer.write(object);
	}

	@Override
	public String writeAll(List<Versioned<DishItem>> objects)
	{
		return serializer.writeAll(objects);
	}
}
