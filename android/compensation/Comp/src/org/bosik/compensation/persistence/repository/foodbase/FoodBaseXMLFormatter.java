package org.bosik.compensation.persistence.repository.foodbase;

import java.io.ByteArrayOutputStream;
import java.io.OutputStream;
import javax.xml.transform.TransformerException;
import javax.xml.transform.dom.DOMSource;
import javax.xml.transform.stream.StreamResult;
import org.bosik.compensation.persistence.entity.foodbase.Food;
import org.bosik.compensation.persistence.entity.foodbase.FoodBase;
import org.bosik.compensation.persistence.repository.common.Serializer;
import org.bosik.compensation.persistence.repository.common.XmlFormatter;
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
public class FoodBaseXMLFormatter extends XmlFormatter implements Serializer<FoodBase>
{
	// TODO: cleanup
	/*@Override
	public int getVersion(String xmlData)
	{
		Document doc = getDocument(xmlData);
		Element root = doc.getDocumentElement();
		return Integer.parseInt(root.getAttribute("version"));
	}*/

	@Override
	public FoodBase read(String xmlData)
	{
		Document doc = getDocument(xmlData);
		Element root = doc.getDocumentElement();
		NodeList nodes = root.getChildNodes();

		FoodBase foodBase = new FoodBase();
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
				food.setFromTable(x.getAttribute("table").equals("True"));

				foodBase.add(food);
			}
		}

		foodBase.endUpdate();
		return foodBase;
	}

	@Override
	public String write(FoodBase foodBase)
	{
		try
		{
			Document doc = builder.newDocument();

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
				childelement.setAttribute("table", String.valueOf(food.getFromTable() ? "True" : "False"));
				root.appendChild(childelement);
			}

			DOMSource source = new DOMSource(doc);
			OutputStream stream = new ByteArrayOutputStream();

			StreamResult result = new StreamResult(stream);
			transformer.transform(source, result);

			return stream.toString();
		} catch (TransformerException ex)
		{
			ex.printStackTrace();
		}
		return null;
	}
}
