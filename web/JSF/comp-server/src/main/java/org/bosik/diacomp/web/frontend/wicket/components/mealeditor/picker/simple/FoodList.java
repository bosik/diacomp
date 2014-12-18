package org.bosik.diacomp.web.frontend.wicket.components.mealeditor.picker.simple;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.List;
import org.bosik.diacomp.core.entities.business.FoodMassed;

public class FoodList implements Serializable
{
	private static final long	serialVersionUID	= 2334881016352583460L;

	private List<FoodMassed>	content				= new ArrayList<FoodMassed>();

	public List<FoodMassed> getContent()
	{
		return content;
	}

	public void setContent(List<FoodMassed> data)
	{
		this.content = data;
	}
}
