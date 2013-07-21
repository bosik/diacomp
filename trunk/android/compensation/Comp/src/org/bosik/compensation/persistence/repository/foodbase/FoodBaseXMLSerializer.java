package org.bosik.compensation.persistence.repository.foodbase;

import org.bosik.compensation.persistence.entity.foodbase.Food;
import org.bosik.compensation.persistence.repository.common.Base;
import org.bosik.compensation.persistence.repository.common.Serializer;
import org.bosik.compensation.persistence.repository.common.XmlFormatter;
import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;
import android.util.Log;

/**
 * Умеет загружать базу продуктов из строки в xml-формате.
 * 
 * Умеет сохранять базу продуктов в строку в xml-формате.
 * 
 * Будет использоваться и в локальном, и в веб-репозитории.
 * 
 * @author Bosik
 * 
 */
@Deprecated
public class FoodBaseXMLSerializer implements Serializer<Base<Food>>
{
	private static final String TAG = FoodBaseXMLSerializer.class.getSimpleName();

	@Override
	public Base<Food> read(String xmlData)
	{
		Log.v(TAG, "Reading: " + xmlData);

		Document doc = XmlFormatter.readDocument(xmlData);
		Element root = doc.getDocumentElement();
		NodeList nodes = root.getChildNodes();

		Base<Food> foodBase = new Base<Food>();
		foodBase.beginUpdate();
		foodBase.setVersion(Integer.parseInt(root.getAttribute("version")));

		for (int i = 0; i < nodes.getLength(); i++)
		{
			Node node = nodes.item(i);

			if (node instanceof Element)
			{
				Element x = (Element) node;

				Food food = new Food();
				food.setName(x.getAttribute("name"));
				food.setRelProts(Double.parseDouble(x.getAttribute("prots")));
				food.setRelFats(Double.parseDouble(x.getAttribute("fats")));
				food.setRelCarbs(Double.parseDouble(x.getAttribute("carbs")));
				food.setRelValue(Double.parseDouble(x.getAttribute("val")));
				food.setFromTable(x.getAttribute("table").equalsIgnoreCase("true"));

				foodBase.add(food);
			}
		}

		foodBase.endUpdate();
		return foodBase;
	}

	@Override
	public String write(Base<Food> foodBase)
	{
		Document doc = XmlFormatter.newDocument();

		Element root = doc.createElement("foods");
		root.setAttribute("version", String.valueOf(foodBase.getVersion()));
		doc.appendChild(root);

		for (int i = 0; i < foodBase.count(); i++)
		{
			Food food = foodBase.get(i);

			Element childelement = doc.createElement("food");
			childelement.setAttribute("name", String.valueOf(food.getName()));
			childelement.setAttribute("prots", String.valueOf(food.getRelProts()));
			childelement.setAttribute("fats", String.valueOf(food.getRelFats()));
			childelement.setAttribute("carbs", String.valueOf(food.getRelCarbs()));
			childelement.setAttribute("val", String.valueOf(food.getRelValue()));
			childelement.setAttribute("table", String.valueOf(food.getFromTable()));
			root.appendChild(childelement);
		}

		return XmlFormatter.writeDocument(doc);
	}
}
