package org.bosik.compensation.persistence.serializers.foodbase;

import java.util.List;
import org.bosik.compensation.bo.foodbase.FoodItem;
import org.bosik.compensation.persistence.common.MemoryBase;
import org.bosik.compensation.persistence.serializers.Serializer;
import org.bosik.compensation.utils.XmlUtils;
import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;

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
public class FoodBaseXMLSerializer implements Serializer<MemoryBase<FoodItem>>
{
	// private static final String TAG = DishBaseXMLSerializer.class.getSimpleName();

	@Override
	public MemoryBase<FoodItem> read(String xmlData)
	{
		// Log.v(TAG, "Reading: " + xmlData);
		if ("".equals(xmlData))
		{
			return new MemoryBase<FoodItem>();
		}

		Document doc = XmlUtils.readDocument(xmlData);
		Element root = doc.getDocumentElement();
		NodeList nodes = root.getChildNodes();

		MemoryBase<FoodItem> foodBase = new MemoryBase<FoodItem>();
		foodBase.beginUpdate();
		foodBase.clear();

		for (int i = 0; i < nodes.getLength(); i++)
		{
			Node node = nodes.item(i);

			if (node instanceof Element)
			{
				Element x = (Element) node;

				FoodItem food = new FoodItem();
				food.setId(x.getAttribute("id"));
				food.setName(x.getAttribute("name"));
				food.setRelProts(Double.parseDouble(x.getAttribute("prots")));
				food.setRelFats(Double.parseDouble(x.getAttribute("fats")));
				food.setRelCarbs(Double.parseDouble(x.getAttribute("carbs")));
				food.setRelValue(Double.parseDouble(x.getAttribute("val")));
				food.setFromTable(x.getAttribute("table").equalsIgnoreCase("true"));

				foodBase.add(food);
			}
		}

		foodBase.setVersion(Integer.parseInt(root.getAttribute("version")));

		foodBase.endUpdate();
		return foodBase;
	}

	@Override
	public String write(MemoryBase<FoodItem> foodBase)
	{
		Document doc = XmlUtils.newDocument();

		Element root = doc.createElement("foods");
		root.setAttribute("version", String.valueOf(foodBase.getVersion()));
		doc.appendChild(root);

		for (int i = 0; i < foodBase.count(); i++)
		{
			FoodItem food = foodBase.get(i);

			Element childelement = doc.createElement("food");
			childelement.setAttribute("id", String.valueOf(food.getId()));
			childelement.setAttribute("name", String.valueOf(food.getName()));
			childelement.setAttribute("prots", String.valueOf(food.getRelProts()));
			childelement.setAttribute("fats", String.valueOf(food.getRelFats()));
			childelement.setAttribute("carbs", String.valueOf(food.getRelCarbs()));
			childelement.setAttribute("val", String.valueOf(food.getRelValue()));
			childelement.setAttribute("table", String.valueOf(food.getFromTable()));
			root.appendChild(childelement);
		}

		return XmlUtils.writeDocument(doc);
	}

	@Override
	public List<MemoryBase<FoodItem>> readAll(String data)
	{
		throw new RuntimeException("Not implemented");
	}

	@Override
	public String writeAll(List<MemoryBase<FoodItem>> object)
	{
		throw new RuntimeException("Not implemented");
	}
}
