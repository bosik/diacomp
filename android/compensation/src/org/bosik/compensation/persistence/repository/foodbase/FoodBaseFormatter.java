package org.bosik.compensation.persistence.repository.foodbase;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.io.UnsupportedEncodingException;
import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.ParserConfigurationException;
import javax.xml.transform.Transformer;
import javax.xml.transform.TransformerFactory;
import javax.xml.transform.dom.DOMSource;
import javax.xml.transform.stream.StreamResult;
import org.bosik.compensation.persistence.entity.foodbase.Food;
import org.bosik.compensation.persistence.entity.foodbase.FoodBase;
import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;
import org.xml.sax.SAXException;

public class FoodBaseFormatter
{
	private static DocumentBuilder builder;
	static
	{
		try
		{
			DocumentBuilderFactory factory = DocumentBuilderFactory.newInstance();
			builder = factory.newDocumentBuilder();
		} catch (ParserConfigurationException e)
		{
			e.printStackTrace();
		}
	}

	private static Document getDocument(String xmlData)
	{
		try
		{
			InputStream stream = new ByteArrayInputStream(xmlData.getBytes("UTF-8"));
			return builder.parse(stream);
		} catch (UnsupportedEncodingException e)
		{
			e.printStackTrace();
		} catch (SAXException e)
		{
			e.printStackTrace();
		} catch (IOException e)
		{
			e.printStackTrace();
		}

		return null;
	}

	public static int getVersion(String xmlData)
	{
		Document dom = getDocument(xmlData);
		Element root = dom.getDocumentElement();
		return Integer.parseInt(root.getAttribute("version"));
	}

	public static FoodBase read(String xmlData)
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

	public static String write(FoodBase foodBase)
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

			TransformerFactory transformerfactory = TransformerFactory.newInstance();
			Transformer transformer = transformerfactory.newTransformer();
			DOMSource source = new DOMSource(doc);
			OutputStream stream = new ByteArrayOutputStream();
			// FileOutputStream stream = getApplicationContext().openFileOutput("NewDom.xml",
			// getApplicationContext().MODE_WORLD_WRITEABLE);
			StreamResult result = new StreamResult(stream);
			transformer.transform(source, result);

			return stream.toString();
		} catch (Exception ex)
		{
			ex.printStackTrace();
		}
		return null;
	}
}
