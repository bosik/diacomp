package org.bosik.compensation.persistence.repository.foodbase;

import org.bosik.compensation.persistence.entity.foodbase.Food;
import org.bosik.compensation.persistence.repository.common.LocalBase;
import android.content.Context;

public class LocalFoodBaseRepository extends LocalBase<Food>
{
	public LocalFoodBaseRepository(Context context, String fileName)
	{
		// один параметр скрываем, подставляя определённый сериализатор
		super(context, fileName, new FoodBaseXMLSerializer());
	}
}