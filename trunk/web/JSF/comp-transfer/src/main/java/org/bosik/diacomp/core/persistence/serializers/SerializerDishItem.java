package org.bosik.diacomp.core.persistence.serializers;

import org.bosik.diacomp.core.entities.business.dishbase.DishItem;
import org.bosik.diacomp.core.entities.tech.Versioned;
import org.bosik.diacomp.core.persistence.parsers.ParserDishItem;
import org.bosik.diacomp.core.persistence.utils.ParserVersioned;
import org.bosik.diacomp.core.persistence.utils.SerializerAdapter;

public class SerializerDishItem extends SerializerAdapter<Versioned<DishItem>>
{
	public SerializerDishItem()
	{
		super(new ParserVersioned<DishItem>(new ParserDishItem()));
	}
}
