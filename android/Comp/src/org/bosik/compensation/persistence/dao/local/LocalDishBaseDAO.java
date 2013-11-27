package org.bosik.compensation.persistence.dao.local;

import java.io.IOException;
import org.bosik.compensation.bo.dishbase.DishItem;
import org.bosik.compensation.persistence.dao.local.utils.FileBaseDAO;
import org.bosik.compensation.persistence.serializers.dishbase.DishBaseXMLSerializer;
import android.content.Context;

public class LocalDishBaseDAO extends FileBaseDAO<DishItem>
{
	public LocalDishBaseDAO(Context context, String fileName) throws IOException
	{
		// hide last parameter with create serializer
		super(context, fileName, new DishBaseXMLSerializer());
	}
}