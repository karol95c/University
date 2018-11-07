using System.Collections;
using System.Collections.Generic;
using UnityEngine;
using System.Security.Cryptography;
using System;

public class PlatformState : MonoBehaviour {

	// Use this for initialization
	public List<CardState> rotatedCards = new List<CardState>();
	public List<Material> materialOptions;
	public List<GameObject> cards;
	void Start () {
		materialOptions.Shuffle();
		foreach (Transform child in transform)
		{
			cards.Add(child.gameObject);
		}
		setMaterial();
	}
	
	public void rotateTwo()
	{
		CardState temp1 = rotatedCards.Dequeue();
		CardState temp2 = rotatedCards.D

		if (temp = )
		rotatedCards.Dequeue().rotateBack = true;
	}

	public int rotatedCount()
	{
		return rotatedCards.Count;
	}
	
	// Update is called once per frame
	void Update () {
	}

	void setMaterial()
	{
		CardState cardscript;
		cards.Shuffle();
		for (int i = 0; i < cards.Count; ++i)
		{
			cardscript = cards[i].GetComponent<CardState>();
			cardscript.setMaterial(materialOptions[i%8]);
		}
	}
}
static class MyExtensions
{
	public static void Shuffle<T>(this IList<T> list)
	{
		RNGCryptoServiceProvider provider = new RNGCryptoServiceProvider();
		int n = list.Count;
		while (n > 1)
		{
			byte[] box = new byte[1];
			do provider.GetBytes(box);
			while (!(box[0] < n * (Byte.MaxValue / n)));
			int k = (box[0] % n);
			n--;
			T value = list[k];
			list[k] = list[n];
			list[n] = value;
		}
	}
}
