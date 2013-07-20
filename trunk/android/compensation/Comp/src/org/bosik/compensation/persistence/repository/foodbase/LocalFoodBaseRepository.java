package org.bosik.compensation.persistence.repository.foodbase;

import org.bosik.compensation.persistence.entity.foodbase.Food;
import org.bosik.compensation.persistence.repository.common.LocalBaseRepository;
import android.content.Context;

public class LocalFoodBaseRepository extends LocalBaseRepository<Food>
{
	public LocalFoodBaseRepository(Context context, String fileName)
	{
		super(context, fileName, new FoodBaseXMLSerializer());
	}
}