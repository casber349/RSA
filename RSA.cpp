#include <iostream>
#include <iomanip>
#include <vector>
#include <random>
#include <assert.h>
#include <bitset> 

using namespace std;

class big_num {
	friend int compare(big_num& A, big_num& B);
	public:
		vector<unsigned int>data;
		
		big_num(){}
		big_num(unsigned int number) {
			data.push_back(number);
		}
		big_num(std::vector<unsigned int>::iterator it, int size) {
			for (int j = 0; j < size; j++) {
				data.push_back(*it);
				it++;
			}
		}

		big_num addition(big_num& operand_R) { 	// addition (operand_L(this) + operand_R)
			big_num result;
			unsigned int carry = 0;
			unsigned int temp;
			unsigned int L_val, R_val;
			int max_size = (int)this->data.size();
			if (max_size < (int)operand_R.data.size()) {
				max_size = (int)operand_R.data.size();
			}

			for (int index = 0; index < max_size; index++) {
				if (index < (int)this->data.size()) {
					L_val = this->data[index];
				}
				else {
					L_val = 0;
				}

				if (index < (int)operand_R.data.size()) {
					R_val = operand_R.data[index];
				}
				else {
					R_val = 0;
				}

				temp = L_val + R_val + carry;
			
				if ((temp >= L_val) && (temp >= R_val)) {
					carry = 0;
				}
				else {	// overflow
					carry = 1;
				}

				result.data.push_back(temp);
			}

			if (carry) {
				result.data.push_back(1);
			}
			return result;
		}

		big_num subtraction_abs(big_num& operand_R) {
			if (compare(*this, operand_R) == -1) {
				return (operand_R.sub_helper(*this));
			}
			return (this->sub_helper(operand_R));
		}

		big_num sub_helper(big_num& operand_R) {		// subtraction (operand_L(this) >= operand_R)
			big_num result;
			unsigned int borrow = 0;
			unsigned int temp;
			unsigned int L_val, R_val;
			int max_size = (int)this->data.size();

			for (int index = 0; index < max_size; index++) {
				if (index < (int)this->data.size()) {
					L_val = this->data[index];
				}
				else {
					L_val = 0;
				}

				if (index < (int)operand_R.data.size()) {
					R_val = operand_R.data[index];
				}
				else {
					R_val = 0;
				}

				temp = L_val - R_val - borrow;

				if (temp > L_val) {	// overflow
					borrow = 1;
				}
				else {
					borrow = 0;
				}

				result.data.push_back(temp);
			}

			for (int index = (int)(result.data.size() - 1); index > 0; index--) {
				if (!result.data[index]) {
					result.data.pop_back();
				}
				else {
					break;
				}
			}
			return result;
		}
		
		int highest_bit_index() {
			if ((this->data.size() == 1) && (!this->data[0])) {
				return -1;
			}
			unsigned int num_mask = 0x80000000;
			int highest_index = 31;
			int size = (int)this->data.size();
			unsigned int highest_unit = this->data[size - 1];

			while (1) {
				if (highest_unit & num_mask) {
					break;
				}
				highest_index--;
				num_mask = num_mask >> 1;
			}
			return ((size - 1) * 32 + highest_index);
		}

		big_num shift_left(int bits) {
			if (!bits) {
				return *this;
			}
			big_num result = *this; //big_num& result = new big_num(*this)
			int shift_units = bits / 32;
			int shift_bits = bits % 32;

			unsigned int shift_in = 0, shift_out = 0;
			for (int j = 0; j < result.data.size(); j++) {
				shift_out = result.data[j] >> (32 - bits);
				result.data[j] <<= bits;
				result.data[j] += shift_in;

				shift_in = shift_out;
			}

			if (shift_out) {
				result.data.push_back(shift_out);
			}

			for (int j = 0; j < shift_units; j++) {
				result.data.insert(result.data.begin(), 0);
			}

			return result;
		}

		big_num shift_right(int bits) {
			big_num result = *this; //big_num& result = new big_num(*this)
			int shift_units = bits / 32;
			int shift_bits = bits % 32;

			for (int j = 0; j < shift_units; j++) {
				result.data.erase(result.data.begin());
			}

			int size = (int)result.data.size();
			unsigned int shift_in = 0, shift_out = 0;
			for (int j = size - 1; j >= 0; j--) {
				shift_out = result.data[j] << (32 - bits);
				result.data[j] >>= bits;
				result.data[j] += shift_in;

				shift_in = shift_out;
			}

			if (!result.data[size - 1]) {
				result.data.pop_back();
			}

			return result;
		}
		/*
		big_num mod(big_num& B) {	// this mod B = R.
			assert(!B.is_zero());
			int cmp_result = compare(*this, B);

			if (is_zero()) {
				big_num zero(0);
				return zero;
			}

			if (cmp_result == 0) {	// operand(this) == mod_num
				big_num zero(0);
				return zero;
			}

			if (cmp_result == -1) {	// operand(this) < mod_num
				return *this;
			}

			int bit_difference = highest_bit_index() - B.highest_bit_index();
			big_num temp = B.shift_left(bit_difference);
			for (; bit_difference >= 0; bit_difference--) {
				if (compare(*this, temp) != -1) {
					*this = subtraction_abs(temp);
				}
				temp = temp.shift_right(1);
			}
			return *this;
		}
		*/
		int div(big_num& B, big_num& Q, big_num& R) {	// this / B = Q ... R. Q(0), R(0)
			int cmp_result = compare(*this, B);
			assert(!B.is_zero());

			if (is_zero()) {
				Q.data.clear();
				R.data.clear();
				Q.data.push_back(0);
				R.data.push_back(0);
				return 0;
			}

			if (cmp_result == 0) {	// operand(this) == mod_num
				Q.data.clear();
				R.data.clear();
				Q.data.push_back(1);
				R.data.push_back(0);
				return 0;
			}

			if (cmp_result == -1) {	// operand(this) < mod_num
				Q.data.clear();
				Q.data.push_back(0);
				R = *this;
				return 0;
			}

			Q.data.clear();
			Q.data.push_back(0);
			int bit_difference = highest_bit_index() - B.highest_bit_index();
			big_num temp = B.shift_left(bit_difference);
			for (; bit_difference >= 0; bit_difference--) {
				Q = Q.shift_left(1);
				if (compare(*this, temp) != -1) {
					*this = subtraction_abs(temp);
					big_num one(1);
					Q = Q.addition(one);
				}
				temp = temp.shift_right(1);
			}

			R = *this;
			return 0;
		}

		bool is_zero() {
			if ((this->data.size() == 1) && (!this->data[0])) {
				return true;
			}
			return false;
		}

		big_num multiplication(big_num& operand_R) {		// multiplication (A * B)
			big_num result(0);
			if (this->is_zero() || operand_R.is_zero()) {
				return result;
			}

			big_num addend;
			for (int index_L = 0; index_L < (int)this->data.size(); index_L++) {
				for (int index_R = 0; index_R < (int)operand_R.data.size(); index_R++) {
					unsigned long long temp = (unsigned long long)this->data[index_L] * (unsigned long long)operand_R.data[index_R];

					for (int j = 0; j < index_L + index_R; j++) {
						addend.data.push_back(0);
					}
					addend.data.push_back((unsigned int)(temp & 0x00000000ffffffff));	// bit 0 ~ 31;
					addend.data.push_back((unsigned int)(temp >> 32));	// bit 32 ~ 63;
					
					result = result.addition(addend);

					addend.data.clear();
				}
			}

			for (int index = (int)(result.data.size() - 1); index > 0; index--) {
				if (!result.data[index]) {
					result.data.pop_back();
				}
				else {
					break;
				}
			}

			return result;
		}

		bool is_this_bit_1(int bit_index) {
			int which_unit = bit_index / 32;
			int which_bit = bit_index % 32;

			if (this->data[which_unit] & (1 << which_bit)) {
				return true;
			}
			return false;
		}

		big_num exp_mod(big_num& E, big_num& N) {	// (this^E) % N
			assert(!N.is_zero());
			big_num result(1);
			if (E.is_zero()) {
				return result;
			}

			big_num Q(0), R(0);
			int E_bits = E.highest_bit_index();
			for (int j = E_bits; j >= 0; j--) {
				result = result.multiplication(result);
				result.div(N, Q, R);
				result = R;
				if (E.is_this_bit_1(j)) {
					result = result.multiplication(*this);
					result.div(N, Q, R);
					result = R;
				}
			}

			return result;
		}

		void randomize(int bits, bool highest_bit_is_1, bool is_odd) {
			int units = bits / 32;
			int extra_bits = bits % 32;
			random_device rd;
			mt19937 gen(rd());
			uniform_int_distribution<unsigned int> dist(1, 0xffffffff);
			data.clear();
			for (int j = 0; j < units; j++) {
				data.push_back(dist(gen));
			}

			if (extra_bits) {
				data.push_back(dist(gen));
				data[(int)data.size() - 1] &= ((1 << extra_bits) - 1);
			}

			if (highest_bit_is_1) {
				if (extra_bits) {
					data[(int)data.size() - 1] |= (1 << (extra_bits - 1));
				}
				else {
					data[(int)data.size() - 1] |= (1 << 31);
				}
			}

			if (is_odd) {
				data[0] |= 1;
			}
		}

		void print_in_binary() {
			int bits = this->highest_bit_index() + 1;
			int units = bits / 32;
			int extra_bits = bits % 32;

			if (data.size()) {
				for (int j = (int)data.size() - 1; j >= 0; j--) {
					cout << bitset<32>(data[j]);
				}
				cout << endl;
			}
		}

		void print_in_hex() {
			for (int j = data.size() - 1; j >= 0; j--) {
				cout << hex << data[j];
			}
			cout << endl;
		}

		int lowest_1_index() {
			int index = 0;
			if ((is_zero()) || (!data.size())) {
				return -1;
			}

			big_num temp = *this;
			while (1) {
				if (temp.data[0] & 1) {
					return index;
				}
				temp = temp.shift_right(1);
				index++;
			}
		}

		bool miller_rabin() {	// Test if (*this) is prime
			assert(data.size());
			if ((data.size() == 1) && (data[0] == 2)) {
				return true;
			}
			if (((data[0] & 1) == 0)) {
				return false;
			}
			
			big_num one(1);
			big_num minus_one;
			big_num Q(0), R(0);
			minus_one = subtraction_abs(one);
			int S_orig = minus_one.lowest_1_index();
			int S = S_orig;
			big_num D = minus_one.shift_right(S_orig);
			for (int j = 0; j < 30; j++) {
				S = S_orig;
				big_num test_num;
				while (1) {
					test_num.randomize(highest_bit_index() - 1, 0, 0);	// 1 < test_num < *this - 1
					if (compare(test_num, one) == 1) {
						break;
					}
				}
				big_num orig = test_num;
				test_num = test_num.exp_mod(D, *this);	// test_num : Y

				while (S >= 0) {
					if (compare(one, test_num) == 0) {	// Y == 1
						break;
					}
					if (S > 0) {
						if (compare(minus_one, test_num) == 0) {	// Y == -1
							break;
						}
					}

					S--;
					test_num = test_num.multiplication(test_num);
					test_num.div(*this, Q, R);
					test_num = R;
				}

				if (S < 0) {
					return false;
				}
			}
			return true;
		}

		big_num gcd(big_num& N) {
			big_num A, B;
			int cmp_result = compare(*this, N);
			if (cmp_result == 0) {
				return *this;
			}
			else if (cmp_result == 1) {
				A = *this;
				B = N;
			}
			else {
				B = *this;
				A = N;
			}


			while (!B.is_zero()) {
				big_num Q(0), R(0);
				A.div(B, Q, R);
				A = B;
				B = R;
			}

			return A;
		}
		
		

		big_num mod_inverse(big_num& P) {	// P is prime
			/*
				A(*this) ^ (P - 1) % P = 1
				A * A ^ (P - 2) % P = 1
				A ^ (P - 2) % P is A's mod inverse
			*/
			big_num A = *this;
			big_num two(2);
			big_num E = P.subtraction_abs(two);
			big_num result;
			result = A.exp_mod(E, P);

			return result;
		}
};

class key_pair {
	public:
		void generate_key_pair(int N_bits) {	// N is prime
			big_num temp;
			while (1) {
				temp.randomize(N_bits, 1, 1);	// temp is odd
				if (temp.miller_rabin()) {
					break;
				}
			}

			N = temp;
			
			E.data.clear();
			E.data.push_back(65537);

			big_num phi_N, one(1);
			phi_N = N.subtraction_abs(one);
			D = E.mod_inverse(phi_N);
		}

		void get_public_part(big_num& n, big_num& e) {
			n = N;
			e = E;
		}

		void get_private_part(big_num& n, big_num& d) {
			n = N;
			d = D;
		}

	private:
		big_num N, E, D;
};

class key_owner {
	public:
		void initialize_key(int bits) {
			key.generate_key_pair(bits);
		}

		big_num sign_data(big_num hashed_message) {
			big_num signature;
			key.get_private_part(N, D);
			signature = hashed_message.exp_mod(D, N);

			return signature;
		}

		void get_public_key(big_num& n, big_num& e) {
			key.get_public_part(n, e);
		}

	private:
		big_num N, D;
		key_pair key;
};

class non_key_owner {
	public:
		void get_public_key_from_key_owner(key_owner& owner) {
			owner.get_public_key(N, E);
		}

		bool verify_signature(big_num signature, big_num hashed_message, big_num* recovered_hash) {
			big_num recovered_message_hash;
			recovered_message_hash = signature.exp_mod(E, N);

			if (recovered_hash) {	// optional
				*recovered_hash = recovered_message_hash;
			}
			
			if (compare(recovered_message_hash, hashed_message) == 0) {
				return true;
			}
			return false;
		}

	private:
		big_num N, E;
};

int compare(big_num& operand_L, big_num& operand_R) {
	if (operand_L.data.size() < operand_R.data.size()) {
		return -1;
	}
	if (operand_L.data.size() > operand_R.data.size()) {
		return 1;
	}
	for (int index = (int)(operand_L.data.size() - 1); index >= 0; index--) {
		if (operand_L.data[index] < operand_R.data[index]) {
			return -1;
		}
		else if (operand_L.data[index] > operand_R.data[index]) {
			return 1;
		}
	}

	return 0;
}

void test_miller_rabin() {
	big_num A(4001), B(4002), C(4003), D(4004), E(4005), F(4006), G(4007);

	cout << A.miller_rabin() << endl;	// T
	cout << B.miller_rabin() << endl;	// F
	cout << C.miller_rabin() << endl;	// T
	cout << D.miller_rabin() << endl;	// F
	cout << E.miller_rabin() << endl;	// F
	cout << F.miller_rabin() << endl;	// F
	cout << G.miller_rabin() << endl;	// T
	// output: 1 0 1 0 0 0 1

}

void test_miller_rabin_2() {
	big_num A(6907), B(6908), C(6909), D(6910), E(6911), F(6912), G(6913);

	cout << A.miller_rabin() << endl;	// T
	cout << B.miller_rabin() << endl;	// F
	cout << C.miller_rabin() << endl;	// F
	cout << D.miller_rabin() << endl;	// F
	cout << E.miller_rabin() << endl;	// T
	cout << F.miller_rabin() << endl;	// F
	cout << G.miller_rabin() << endl;	// F
}

void test_gcd() {
	big_num A(4003 * 4001), B(4007 * 4003);
	big_num C;
	C= A.gcd(B);
	cout << C.data[0] << endl;
}

void test_mod_inverse() {
	big_num A(65537), P, B;
	while (1) {
		P.randomize(40, 0, 0);
		if (P.miller_rabin()) {
			break;
		}
	}
	B = A.mod_inverse(P);
	//cout << A.data[0] << " " << B.data[0]  << " " << P.data[0] << endl;
}

bool egcd(int A, int B, int& gcd, int& inverse) {
	int Q = 0, R = 0;
	// Xn*A(phi_N) + Yn*B(E) = Rn ...-> gcd(A, B)
	int X, X_prev1 = 0, X_prev2 = 1, Y, Y_prev1 = 1, Y_prev2 = 0;
	// X = X_prev2 - Q * X_prev1, Y = Y_prev2 - Q * Y_prev1;

	while (B > 0) {
		Q = A / B;
		R = A % B;

		X = X_prev2 - Q * X_prev1;
		Y = Y_prev2 - Q * Y_prev1;

		if (R == 1) {	// inverse = Y
			inverse = Y;	// found inverse
			gcd = 1;
			return true;
		}

		A = B;
		B = R;

		X_prev2 = X_prev1;
		X_prev1 = X;
		Y_prev2 = Y_prev1;
		Y_prev1 = Y;
	}

	

	gcd = A;
	inverse = 0;

	return false;
}

void test_egcd() {
	int A = 12, B = 9, gcd, inverse;

	egcd(A, B, gcd, inverse);
}

int main() {
	//test_miller_rabin();
	//test_miller_rabin_2();
	test_egcd();
	//test_mod_inverse(); 

	key_owner A;
	non_key_owner B;
	vector<unsigned int> temp;
	temp.push_back(0x12345678);

	big_num orig_message_hash(temp.begin(), (int)temp.size());
	big_num signature, recovered_message;
	bool verify_pass;

	A.initialize_key(32);
	signature = A.sign_data(orig_message_hash);

	cout << "Original message: ";
	orig_message_hash.print_in_hex();
	cout << "Signature: ";
	signature.print_in_hex();

	B.get_public_key_from_key_owner(A);
	verify_pass = B.verify_signature(signature, orig_message_hash, &recovered_message);

	cout << "Recovered message: ";
	recovered_message.print_in_hex();
	
	cout << "Signature varification " << (verify_pass ? "pass!" : "fail!" ) << endl;

	return 0;
}