using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public class BrickScript : MonoBehaviour {

	// Use this for initialization
	public GameObject brickExplodePrefab;
	public Transform brickExplodeSpawn;
	void Start () {

	}
	
	// Update is called once per frame
	void Update () {

	}

	public void Explode()
	{
		var brickExplode = (GameObject)Instantiate (
			brickExplodePrefab,
			brickExplodeSpawn.position,
			brickExplodeSpawn.rotation);
		brickExplode.GetComponent<ParticleSystem>().Play();
		brickExplode.GetComponent<AudioSource>().Play();
		Destroy(brickExplode, 1.0f);
		Destroy(gameObject);

	}

	
}
