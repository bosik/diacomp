package org.bosik.diacomp.core.persistence.serializers;

import org.bosik.diacomp.core.entities.business.foodbase.FoodItem;
import org.bosik.diacomp.core.entities.tech.Versioned;
import org.bosik.diacomp.core.persistence.parsers.ParserFoodItem;
import org.bosik.diacomp.core.persistence.utils.ParserVersioned;
import org.bosik.diacomp.core.persistence.utils.SerializerAdapter;

public class SerializerFoodItem extends SerializerAdapter<Versioned<FoodItem>>
{
	public SerializerFoodItem()
	{
		super(new ParserVersioned<FoodItem>(new ParserFoodItem()));
	}
}
