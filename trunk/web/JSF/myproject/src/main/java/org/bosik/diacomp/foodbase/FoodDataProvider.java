package org.bosik.diacomp.foodbase;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import org.apache.wicket.markup.repeater.data.IDataProvider;
import org.apache.wicket.model.IModel;
import org.apache.wicket.model.Model;

public class FoodDataProvider implements IDataProvider<Food>
{
	private static final long	serialVersionUID	= 1L;

	private static List<Food>	data;
	{
		data = new ArrayList<Food>();
		data.add(new Food("1", "Test food one"));
		data.add(new Food("2", "Test food number two"));
		data.add(new Food("3", "Test food number three"));
	}

	@Override
	public void detach()
	{
	}

	@Override
	public Iterator<? extends Food> iterator(long first, long count)
	{
		int iFirst = (int) first;
		int iCount = (int) count;
		return data.subList(iFirst, iFirst + iCount).iterator();
	}

	@Override
	public long size()
	{
		return data.size();
	}

	@Override
	public IModel<Food> model(Food object)
	{
		// return new DetachableFoodModel(object);
		return new Model<Food>(object);
	}
}
